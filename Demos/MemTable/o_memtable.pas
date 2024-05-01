unit o_MemTable;

{$mode objfpc}{$H+}

//{$DEFINE CURSOR_DEBUG}
{$DEFINE STRING_BLOBS}      { see comments below for Blob buffer layout }
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
   Classes
  ,SysUtils
  ,DateUtils
  //,MaskUtils
  ,SyncObjs
  ,Variants
  //,Generics.Collections
  //,FGL
  //,Contnrs
  //,TypInfo

  ,DB
  ,FmtBCD

  ,dbf_prscore
  //,dbf_prsdef

  ,o_FilterParser
  ;




type
  TMemTableSortMode  = (smNone, smAsc, smDesc);

(*============================================================================
                           Record buffer layout
 -----------------------------------------------------------------------------


 +-----------------------+-----------------------+-----------------------+-----------------------+
 |                       |                       |                       |                       |
 |      Data Fields      |   Calculated Fields   |      Blob fields      |       TRecInfo        |
 |                       |                       |                       |                       |
 +-----------------------+-----------------------+-----------------------+-----------------------+
 0                       CalcOfs                 BlobOfs                 BookOfs


 ----------------------------------------------------------------------------
 Data fields         : fkData and fkInternalCalc fixed length fields
 Calculated fields   : fkLookup and fkCalculated fields
 Blob fields         : pointers to the actual blobs
 ----------------------------------------------------------------------------
 Field buffer layout : byte  0         is the Null Flag Byte (0 = null, else not null)
                       bytes 1..n      field data
 ----------------------------------------------------------------------------
 Blob buffer layout  : byte  0         is the Null Flag Byte (0 = null, else not null)
                       bytes 1..n      blob data

                       see TBlob below, which is used in accessing blobs.

 NOTE (for blobs)    : the compiler directive STRING_BLOBS controls the
                       type of the TBlob.Data, that is the data type of the storage buffer
                       where the actual blob data is stored.
                       This could be either a string or a dynamic array of byte.
                       It seems that string is a little bit faster than
                       the dynamic byte array.

                       Except of strings and dynamic byte arrays, I tried
                       the classic approach, that is, plain pointer buffers
                       and AllocMem, FreeMem, Move etc. It turned out that
                       this last appoach is the slower. So choose either string
                       or dynamic array of byte.
 ----------------------------------------------------------------------------
 TRecInfo layout      : see below
 ============================================================================*)

type
  TFieldBuffer = Pointer;

type
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Bookmark       : Integer;         // the BookMark
    BookmarkFlag   : TBookmarkFlag;   // = (bfCurrent, bfBOF, bfEOF, bfInserted)
    Status         : TUpdateStatus;   // = (usUnmodified, usModified, usInserted, usDeleted)
  end;


type
   PBlob = ^TBlob;
   TBlob = packed record
     Flag   : Byte;
     Data   : string;
   end;

   TBlobs = array[0..High(Word)] of TBlob;
   PBlobs = ^TBlobs;

 type
    { used in comparing fields in order to break the comparison loops }
    TBreakMode    = ( bmG
                     ,bmGE
                     ,bmNE
                     ,bmL
                     ,bmLE
                     );

type
   { the cursor may be in one or more modes }
   TCursorMode = ( cmStatus
                   ,cmLink
                   ,cmRange
                   ,cmSort
                   ,cmFilter
                  );

   TCursorModes = set of TCursorMode;

type
   TKeyBufferIndex = (biMaster
                     ,biRangeStart
                     ,biRangeEnd
                     ,biLocate
                     );

type
  TMemTable = class;

 type
   { TFieldInfo }
   TFieldInfo = class(TObject)
   protected
     FDataset           : TMemTable;
     FField             : TField;
     function  GetIsAutoInc: Boolean;
   public
     constructor Create(Field: TField);

     property Field        : TField read FField;
     property IsAutoInc    : Boolean read GetIsAutoInc;
     //property Value        : Variant read GetValue write SetValue;
   end;


type
  { TMemTable }
  TMemTable = class(TDataSet)
  private
    procedure SetCurRecIndex(Value: Integer);
  private
    FSortMode: TMemTableSortMode;
    FSortOnFieldNames: string;
    FTableName                 : string;
    FLock                      : SyncObjs.TCriticalSection;
    FLockCount                 : Integer;

    FAllRows                   : TList;   { all rows }
    FRows                      : TList;   { current rows }

    FFields                    : TList;   { all TFieldInfo fields - owned }
    FBlobFields                : TList;   { TFieldInfo list }
    FSortOnFields              : TList;   { TFieldInfo list }

    FDetailFields              : TList;   { TFieldInfo list }
    FRangeFields               : TList;   { TFieldInfo list }
    FModifiedFields            : TList;   { TField list }

    { size indicator }
    FRecBufSize                : Integer;                 { record buffer total size - all fields + SizeOf(TRecInfo) }

    { offsets }
    FCalcOfs                   : Integer;
    FBlobOfs                   : Integer;
    FBookOfs                   : Integer;

    { field attribute arrays }
    FFieldTypes                : array of TFieldType;
    FFieldBufferSizes          : array of Integer;        { data size of field }
    FOffsets                   : array of Integer;        { offset of a field data. The first byte is the Null Flag  byte }

    FCurRecIndex               : Integer;                 { the current record index in the FRows list }
    FLastBookmark              : Integer;                 { an auto inc number unique for each record, stored in TRecInfo.Bookmark }

    FModes                     : TCursorModes;
    FStatusOptions             : TUpdateStatusSet;
    FKeyBuffers                : array[TKeyBufferIndex] of PChar;

    FFilterParser              : TFilterParser;
    FFilterBuffer              : TRecordBuffer;
    FEmptyBlob                 : TBlob;

    FMasterLink                : TMasterDataLink;
    FDetailFieldNames          : string;

    FIsInRange                 : Boolean;
    FRangeExclusive            : Boolean;
    FInitialized               : Boolean;
    FIsFilterActive            : Boolean;

    function  GetMasterDataSource: TDataSource;
    function  GetMasterFieldNames: string;
    function  GetStatusFilter: TUpdateStatusSet;
    procedure SetDetailFieldNames(Value: string);
    procedure SetIsFilterActive(Value: Boolean);
    procedure SetSortMode(Value: TMemTableSortMode);
    procedure SetSortOnFieldNames(Value: string);
    procedure SetMasterDataSource(Value: TDataSource);
    procedure SetMasterFieldNames(Value: string);
    procedure SetStatusFilter(Value: TUpdateStatusSet);

    property CurRecIndex: Integer read FCurRecIndex write SetCurRecIndex;

    procedure Lock;
    procedure UnLock;

    { initialization }
    procedure Initialize;
    procedure Finalize;
    procedure DeleteRows;

    { get/set field data }
    function  GetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; const FieldIndex: Integer): Boolean;
    procedure SetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; const FieldIndex: Integer);

    function  IsFieldBufferNull(FieldBuf: TFieldBuffer; Size: Integer): Boolean;

    { blobs }
    procedure InitializeBlobs(RecBuf: TRecordBuffer);
    procedure FinalizeBlobs(RecBuf: TRecordBuffer);

    procedure FreeBlobs(RecBuf: TRecordBuffer);
    procedure FreeBlob(RecBuf: TRecordBuffer; FieldIndex: Integer);
    procedure CopyBlobs(SourceRecBuf, DestRecBuf: TRecordBuffer);
    procedure CopyBlob(SourceRecBuf, DestRecBuf: TRecordBuffer; FieldIndex: Integer);

    { record }
    procedure CopyRecord(SourceRecBuf, DestRecBuf: TRecordBuffer);
    function  CanDisplayRecord(RecBuf: TRecordBuffer): Boolean;
    function  GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;

    { bookmark }
    function  GoToBookmarkInternal(BM: Integer): Boolean;
    function  IndexOfBookmark(BM: Integer): Integer;
    function  GetBookmarkInternal(RecBuf: TRecordBuffer): Integer;

    { TFieldInfo related }
    procedure AddFields();

    procedure GetFieldNames(List: TStrings);
    function  IndexOfField(Field: TFieldInfo): Integer;
    function  IndexOfFieldName(const FieldName: string): Integer;
    function  FindInfoField(const FieldName: string): TFieldInfo;
    function  GetFieldInfoByName(const FieldName: string): TFieldInfo;

    procedure GetFieldInfoList(List: TList; const FieldNames: string); overload;
    function  GetFieldInfoList(const FieldNames: string): TList; overload;

    function  GetFieldBufferSize(FieldInfo: TFieldInfo): Integer;

    { blobs }
    function  GetBlobSize(RecBuf: PChar; FieldIndex: Integer): LongWord;
    function  GetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer): LongWord;
    procedure SetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer; BlobSize: LongWord);

    { comparing-sorting }
    function  CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; Options: TLocateOptions): Integer;
    function  CompareRecords(const Buf1, Buf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer;
    procedure QuickSort(L, R: Integer; const RowList: TList; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode);

    { records - miscs }
    function  GetUpdateStatus(RecBuf: TRecordBuffer): TUpdateStatus;
    function  GetRecordByIndex(RecBuf: TRecordBuffer; RecordIndex: Integer): Boolean;
    function  LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; var RecIndex: Integer): Boolean; overload;
    function  LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;  overload;

    { field get/set value as Variant }
    function  GetValueFromRecBuf(RecBuf: TRecordBuffer; const FieldIndex: Integer): Variant;
    procedure SetValueToRecBuf(RecBuf: TRecordBuffer; const FieldIndex: Integer; const Value: Variant);
    function  GetValueFromFieldBuf(FieldBuf: TFieldBuffer; const FieldIndex: Integer): Variant;
    procedure SetValueToFieldBuf(FieldBuf: TFieldBuffer; const FieldIndex: Integer; Value: Variant);

    { master-detail }
    procedure OnMasterLinkChanged(Sender: TObject);
    procedure OnMasterLinkDisabled(Sender: TObject);
    procedure LoadDetailFieldList();

    { operations }
    procedure SetLink(Value: Boolean);
    procedure Sort(); overload;

    { miscs }
    procedure LoadIndexFieldList();
    procedure VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant);

    procedure OnFilterVariableValueEvent(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
  protected
    {== TDataset overrides ==}

    { record buffer }
    function  AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
    function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;

    { bookmark }
    function  GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(pBM: Pointer); override;
    procedure InternalSetToRecord(RecBuf: TRecordBuffer); override;

    { navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;

    { editing }
    procedure InternalInitRecord(RecBuf: TRecordBuffer); override;
    procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;

    { open/close }
    function  IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

    { filter }
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    { filter - own }
    function  CanActivateFilter(): Boolean;
    property  IsFilterActive: Boolean read FIsFilterActive write SetIsFilterActive;
    function  FilterCanDisplayRecord(RecBuf: TRecordBuffer): Boolean;

    { miscs }
    procedure InternalInitFieldDefs; override;
    procedure ClearCalcFields(RecBuf: TRecordBuffer); override;
    procedure InternalHandleException; override;

    procedure DoOnNewRecord; override;
    procedure DoAfterScroll; override;

    { optional }
    function  GetRecordCount: Integer; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {== TDataset overrides ==}
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function  BookmarkValid(BM: TBookmark): Boolean; override;

    function  Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function  Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function  UpdateStatus: TUpdateStatus; override;

    { additional }
    procedure CreateDataset;
    procedure EmptyDataSet;

    procedure SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
    procedure CancelRange;

    procedure CancelUpdates();

    procedure Sort(FieldName: string; SortMode: TMemTableSortMode = smAsc);  overload;
    procedure NextSort(FieldName: string);
    procedure Rebuild;

    { supported field types }
    class function IsSupportedFieldType(FieldType: TFieldType): Boolean;

    class function IsStringFieldType(FieldType: TFieldType): Boolean;
    class function IsWideStringFieldType(FieldType: TFieldType): Boolean;
    class function IsIntegerFieldType(FieldType: TFieldType): Boolean;
    class function IsFloatFieldType(FieldType: TFieldType): Boolean;
    class function IsBCDFieldType(FieldType: TFieldType): Boolean;
    class function IsDateTimeFieldType(FieldType: TFieldType): Boolean;
    class function IsVariantFieldType(FieldType: TFieldType): Boolean;
    class function IsBlobFieldType(FieldType: TFieldType): Boolean;

    { helpers }
    class function  DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
    class function  NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
    class function  CompareDateTimes(const A, B: TDateTime): Integer;
    class function  BufferToWide(Buffer: Pointer): WideString;
    class procedure WideToBuffer(WS: WideString; Buffer: Pointer);
    class procedure GetFloatFormatAndDigits(CurrencyFormat: Boolean; var FloatFormat: TFloatFormat; var Digits: Integer);
    class procedure ClearObjectList(List: TList);
    class function  Min(const A, B: Integer): Integer;
    class function  Max(const A, B: Integer): Integer;

    { properties }
    property MasterSource      : TDataSource read GetMasterDataSource write SetMasterDataSource;
    property MasterFieldNames  : string read GetMasterFieldNames write SetMasterFieldNames;
    property DetailFieldNames  : string read FDetailFieldNames  write SetDetailFieldNames;
    property SortOnFieldNames  : string read FSortOnFieldNames write SetSortOnFieldNames;
    property SortMode          : TMemTableSortMode read FSortMode write SetSortMode;
    property StatusFilter      : TUpdateStatusSet read GetStatusFilter write SetStatusFilter;

    property IsInRange         : Boolean read FIsInRange;
    property ModifiedFields    : TList read FModifiedFields;    // TField list - valid for the current record only
  published
    property Active;
    property TableName: string read FTableName write FTableName;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;

  end;



implementation

uses
  DBConst
  ;











{ TFieldInfo }
constructor TFieldInfo.Create(Field: TField);
begin
  inherited Create();
  Self.FField := Field;
end;
function TFieldInfo.GetIsAutoInc: Boolean;
begin
  Result := Field.DataType = ftAutoInc;
end;











type
  { TBlobStream }

  TBlobStream = class(TMemoryStream)
  protected
    FFieldIndex  : Integer;
    FField       : TBlobField;
    FMode        : TBlobStreamMode;
    FModified    : Boolean;
    FRecBuf      : TRecordBuffer;
    FDataset     : TMemTable;
  public
    constructor Create(Dataset: TMemTable; Field: TBlobField; RecBuf: TRecordBuffer; Mode: TBlobStreamMode);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
  end;




{ TBlobStream }

constructor TBlobStream.Create(Dataset: TMemTable; Field: TBlobField; RecBuf: TRecordBuffer; Mode: TBlobStreamMode);
var
  BlobSize : LongWord;
begin
  inherited Create();

  FDataset     := Dataset;
  FField       := Field;
  FRecBuf      := RecBuf;
  FMode        := Mode;
  FFieldIndex  := Dataset.IndexOfFieldName(FField.FieldName);


  if Assigned(FRecBuf) then
  begin

    if FMode <> bmRead then
    begin
      if FField.ReadOnly then
        DatabaseErrorFmt(SReadOnlyField, [FField.DisplayName], FField.DataSet);
      if not (FField.DataSet.State in [dsEdit, dsInsert, dsNewValue]) then
        DatabaseErrorFmt(SNotEditing, [Dataset.Name], FField.DataSet);
    end;

    if FMode = bmWrite then    { truncate the blob data }
    begin
      Clear;
      FModified := True;
    end else  begin             { read the blob data }
      BlobSize := FDataset.GetBlobSize(FRecBuf, FFieldIndex);
      if BlobSize > 0 then
      begin
        Self.Position := 0;
        SetSize(BlobSize);
        FDataset.GetBlobData(FRecBuf, Memory, FFieldIndex);
      end;
    end;

  end;


end;
destructor TBlobStream.Destroy;
begin

  if FModified then
  try
    Position := 0;
    FDataset.SetBlobData(FRecBuf, Memory, FFieldIndex, Self.Size);
    FField.Modified := True;
    FDataset.DataEvent(deFieldChange, PtrInt(FField));
  except
    if Assigned(Classes.ApplicationHandleException) then
      Classes.ApplicationHandleException(Self);
  end;

  inherited Destroy;
end;

function TBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;














{ TMemTable }

constructor TMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLock              := SyncObjs.TCriticalSection.Create();

  FAllRows           := TList.Create;
  FRows              := TList.Create;

  FFields            := TList.Create();
  FBlobFields        := TList.Create();
  FSortOnFields      := TList.Create();
  FDetailFields      := TList.Create();
  FRangeFields       := TList.Create();
  FModifiedFields    := TList.Create();

  FMasterLink                 := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange  := @OnMasterLinkChanged;
  FMasterLink.OnMasterDisable := @OnMasterLinkDisabled;

  FStatusOptions    := [usModified, usInserted, usUnmodified];

  FFilterParser      := TFilterParser.Create();
  FFilterParser.OnVariable := @OnFilterVariableValueEvent;

  FCurRecIndex      := -1;
end;

destructor TMemTable.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FFilterParser);

  FreeAndNil(FModifiedFields);
  FreeAndNil(FRangeFields);
  FreeAndNil(FDetailFields);
  FreeAndNil(FSortOnFields);
  FreeAndNil(FBlobFields);
  FreeAndNil(FFields);

  FreeAndNil(FRows);
  FreeAndNil(FAllRows);

  FLock.Free;
end;

function TMemTable.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
  InternalInitRecord(Result);

  if FBlobFields.Count > 0 then
    InitializeBlobs(Result);
end;

procedure TMemTable.FreeRecordBuffer(var RecBuf: TRecordBuffer);
begin
  if FBlobFields.Count > 0 then
  begin
    FreeBlobs(RecBuf);
    FinalizeBlobs(RecBuf);
  end;

  FreeMem(RecBuf);

  RecBuf := nil;
end;
function TMemTable.GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  SourceRecBuf  : TRecordBuffer;
  RecCount      : Integer;
begin
  Lock();
  try
    RecCount := FRows.Count;

    SourceRecBuf := nil;

    if RecCount < 1 then
    begin
       Result := grEOF
    end else begin

       Result := grOk;

       case GetMode of
         gmPrior   : begin
                       if CurRecIndex <= 0 then
                         Result := grBOF
                       else
                         CurRecIndex := CurRecIndex - 1;
                     end;

         gmCurrent : begin
                       if (CurRecIndex < 0) or (CurRecIndex >= RecCount) then
                         Result := grError;
                     end;

         gmNext    : begin
                       if CurRecIndex >= RecCount - 1 then
                         Result := grEOF
                       else
                         CurRecIndex := CurRecIndex + 1;
                     end;

       end;

       if (Result = grOK) and Assigned(RecBuf) then
       begin
         SourceRecBuf := FRows[CurRecIndex];
         CopyRecord(SourceRecBuf, RecBuf);

         PRecInfo(RecBuf + FBookOfs)^.Bookmark        := PRecInfo(SourceRecBuf + FBookOfs)^.Bookmark;
         PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag    := bfCurrent;
         PRecInfo(RecBuf + FBookOfs)^.Status          := PRecInfo(SourceRecBuf + FBookOfs)^.Status;

       end else if (Result = grError) and DoCheck then
         DatabaseError('No records');
     end;

  finally
    UnLock();
  end;

end;

function TMemTable.GetRecordSize: Word;
begin
  Result := FRecBufSize;
end;
function TMemTable.GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
end;
procedure TMemTable.SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag := Value;
end;
procedure TMemTable.GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  BM: Integer;
begin
  BM := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
  PInteger(Data)^ := BM;
end;
procedure TMemTable.SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  BM: Integer;
begin
  BM := PInteger(Data)^;
  PRecInfo(RecBuf + FBookOfs)^.Bookmark := BM;
end;
procedure TMemTable.InternalGotoBookmark(pBM: Pointer);
var
  BM: Integer;
begin
  BM := PInteger(pBM)^;
  GoToBookmarkInternal(BM);
end;
procedure TMemTable.InternalSetToRecord(RecBuf: TRecordBuffer);
var
  BM: Integer;
begin
  BM := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
  GoToBookmarkInternal(BM);
end;
function TMemTable.GoToBookmarkInternal(BM: Integer): Boolean;
var
  Index  : Integer;
begin
  Result := False;
  Lock();
  try
    Index := IndexOfBookmark(BM);

    if Index <> -1 then
    begin
      Result := True;
      CurRecIndex := Index;
    end;

  finally
    UnLock();
  end;
end;
function TMemTable.GetBookmarkInternal(RecBuf: TRecordBuffer): Integer;
begin
  Result := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
end;
function TMemTable.IndexOfBookmark(BM: Integer): Integer;
var
  i     : Integer;
begin
  Lock();
  try
    Result := -1;
    if BM > 0 then
    begin
      for i := 0 to FRows.Count - 1 do
        if (BM = PRecInfo(PChar(FRows[i]) + FBookOfs)^.Bookmark) then
        begin
          Result := i;
          Break; //==>
        end;
    end;
  finally
    UnLock();
  end;

end;

procedure TMemTable.InternalFirst;
begin
  CurRecIndex := -1;
end;
procedure TMemTable.InternalLast;
begin
  CurRecIndex := FRows.Count;
end;
procedure TMemTable.SetCurRecIndex(Value: Integer);
begin
  if FCurRecIndex <> Value then
     FCurRecIndex := Value;
end;

procedure TMemTable.InternalInitRecord(RecBuf: TRecordBuffer);
begin
  FillChar(RecBuf^, FRecBufSize, 0);
end;
procedure TMemTable.InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean);

  function GetNextAutoIncValue(FieldIndex: Integer): Integer;
  var
    i             : Integer;
    RecBuf2       : TRecordBuffer;
    FieldBuf      : TFieldBuffer;
    V             : Integer;
  begin
    Result := 0;

    for i := 0 to FAllRows.Count -1 do
    begin
      RecBuf2 := TRecordBuffer(FAllRows[i]);
      FieldBuf  := RecBuf2 + FOffsets[FieldIndex];
      if not IsFieldBufferNull(FieldBuf, FFieldBufferSizes[FieldIndex]) then
      begin
        //Inc(FieldBuf);
        V := PInteger(FieldBuf)^;
        Result := Max(Result, V);
      end;
    end;

    Result := Result + 1;
  end;

  procedure AssignAutoIncValues();
  var
    i             : Integer;
    FieldInfo     : TFieldInfo;
    Field         : TField;
    P             : TFieldBuffer;
    V             : Integer;
    //Flag          : Boolean;
  begin
    for i := 0 to FFields.Count - 1 do
    begin
      FieldInfo := TFieldInfo(FFields[i]);
      if FieldInfo.IsAutoInc then
      begin
        Field := FieldInfo.Field;
        V :=  GetNextAutoIncValue(Field.Index);
        P := RecBuf + FOffsets[Field.Index];
        //Byte(P[0])    := Byte(1);

        //Inc(P);
        //Move(Buffer^, P^, FFieldBufferSizes[Field.Index])
        //PInteger(P)^ := V;
        Integer(P^) := V;
        //Flag := IsFieldBufferNull(P, FFieldBufferSizes[Field.Index]);
      end;
    end;
  end;

var
  DestRecBuf    : TRecordBuffer;
  RecIndex      : Integer;
begin
  Lock();
  try
    if CurRecIndex < 0 then
       CurRecIndex := 0;

    AssignAutoIncValues();

    Inc(FLastBookmark);

    DestRecBuf     := AllocRecordBuffer();
    CopyRecord(TRecordBuffer(RecBuf), DestRecBuf);

    PRecInfo(RecBuf + FBookOfs)^.Bookmark      := FLastBookmark;
    PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag  := bfCurrent;
    PRecInfo(RecBuf + FBookOfs)^.Status        := usInserted;

    PRecInfo(DestRecBuf + FBookOfs)^.Bookmark      := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
    PRecInfo(DestRecBuf + FBookOfs)^.BookmarkFlag  := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
    PRecInfo(DestRecBuf + FBookOfs)^.Status        := PRecInfo(RecBuf + FBookOfs)^.Status;

    FAllRows.Add(DestRecBuf);

    if CanDisplayRecord(DestRecBuf) then
    begin
      RecIndex := CurRecIndex;
      if RecIndex < 0 then
        RecIndex := 0;

      if IsAppend or (FRows.Count <= 0) then
      begin
        FRows.Add(DestRecBuf);
        InternalLast();
      end else begin
        FRows.Insert(RecIndex, DestRecBuf);
      end;

      if cmSort in FModes then
        Sort();
    end;

  finally
    UnLock();
  end;

end;
procedure TMemTable.InternalPost;
var
  RecBuf          : TRecordBuffer;
  DestRecBuf      : TRecordBuffer;
begin
  inherited InternalPost(); // checks required fields

  Lock();
  try
    RecBuf := ActiveBuffer();

    if State = dsEdit then
    begin
      DestRecBuf := FRows[CurRecIndex];
      CopyRecord(RecBuf, DestRecBuf);

      PRecInfo(DestRecBuf + FBookOfs)^.Bookmark      := PRecInfo(RecBuf + FBookOfs)^.Bookmark;
      PRecInfo(DestRecBuf + FBookOfs)^.BookmarkFlag  := PRecInfo(RecBuf + FBookOfs)^.BookmarkFlag;
      if PRecInfo(DestRecBuf + FBookOfs)^.Status = usUnmodified then
        PRecInfo(DestRecBuf + FBookOfs)^.Status := usModified;

      if not CanDisplayRecord(DestRecBuf) then
      begin
        FRows.Remove(DestRecBuf);
        CurRecIndex := Min(FRows.Count - 1, CurRecIndex)
      end else begin
        Sort();
      end;
    end else begin
        InternalAddRecord(RecBuf, False);
    end;

    FModifiedFields.Clear;

  finally
    UnLock();
  end;

end;

procedure TMemTable.InternalEdit;
begin
  inherited InternalEdit;
  FModifiedFields.Clear;
end;

procedure TMemTable.InternalCancel;
begin
  inherited InternalCancel;
  FModifiedFields.Clear;
  FreeBlobs(ActiveBuffer);
end;

procedure TMemTable.InternalDelete;
var
  RecBuf    : TRecordBuffer;
  RowStatus : TUpdateStatus;
begin
  Lock();
  try
    RecBuf := FRows[CurRecIndex];

    RowStatus := GetUpdateStatus(RecBuf);    // usUnmodified, usModified, usInserted, usDeleted

    if RowStatus in [usInserted] then
    begin
      FAllRows.Remove(RecBuf);
      FreeRecordBuffer(RecBuf);
    end else begin
      PRecInfo(RecBuf + FBookOfs)^.Status := usDeleted;  // keep deleted rows in AllRows
    end;

    FRows.Delete(CurRecIndex);
    CurRecIndex := Min(FRows.Count - 1, CurRecIndex);

  finally
    UnLock();
  end;

end;
function TMemTable.IsCursorOpen: Boolean;
begin
  Result := FInitialized;
end;
procedure TMemTable.InternalOpen;
var
  i : Integer;
begin
  for i := 0 to FieldCount - 1 do
    if not IsSupportedFieldType(Fields[i].DataType) then
      DatabaseErrorFmt(SUnsupportedFieldType, [Fields[i].DisplayName], Self);

  InternalInitFieldDefs();

  if DefaultFields then
    CreateFields;

  BindFields(True);

  AddFields();

  LoadDetailFieldList;
  LoadIndexFieldList;

  Initialize();

end;
procedure TMemTable.InternalClose;
begin
  Finalize();

  if DefaultFields then
    DestroyFields;
end;

procedure TMemTable.SetFiltered(Value: Boolean);
begin
  if (Filtered <> Value) then
  begin
    inherited SetFiltered(Value);
    IsFilterActive := CanActivateFilter();
  end;
end;

procedure TMemTable.SetFilterText(const Value: string);
begin
  if (Filter <> Value) then
  begin
    inherited SetFilterText(Value);
    IsFilterActive := CanActivateFilter();
  end;
end;

procedure TMemTable.SetFilterOptions(Value: TFilterOptions);
begin
  if (FilterOptions <> Value) then
  begin
    inherited SetFilterOptions(Value);
    IsFilterActive := CanActivateFilter();
  end;
end;

procedure TMemTable.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if (OnFilterRecord <> Value) then
  begin
    inherited SetOnFilterRecord(Value);
    IsFilterActive := CanActivateFilter();
  end;
end;

function TMemTable.CanActivateFilter(): Boolean;
begin
  Result := Filtered and ((Length(Filter) > 0) or Assigned(OnFilterRecord));
end;

function TMemTable.FilterCanDisplayRecord(RecBuf: TRecordBuffer): Boolean;
var
  V : Variant;
begin
  V := FFilterParser.Evaluate(RecBuf);
  Result := VarIsBool(V) and (V = True);
end;

procedure TMemTable.SetIsFilterActive(Value: Boolean);
begin
  if FIsFilterActive <> Value then
  begin
    if IsCursorOpen() then
    begin
      if not Value then
      begin
        FIsFilterActive := False;
        FModes := FModes - [cmFilter];


        if Assigned(FFilterBuffer) then
           FFilterBuffer := nil;
      end
      else if CanActivateFilter() then
      begin
        FIsFilterActive := True;
        FModes := FModes + [cmFilter];

        if (Length(Filter) > 0) then
        begin
          FFilterParser.CaseSensitive := foCaseInsensitive in FilterOptions;
          FFilterParser.Parse(Filter);
        end;
      end;

      Rebuild();
    end;
  end;
end;
function TMemTable.CanDisplayRecord(RecBuf: TRecordBuffer): Boolean;
var
  RRS, RRE : Integer; // Range Results
  TempStatusOptions : TUpdateStatusSet;
  Accept : Boolean;
  SavedState : TDataSetState;
begin

  if not FInitialized then
    Exit(False);  //==>

  { status }
  if (cmStatus in FModes) then
    TempStatusOptions := FStatusOptions
  else
    TempStatusOptions := [usModified, usInserted, usUnmodified];

  if not (PRecInfo(RecBuf + FBookOfs)^.Status in TempStatusOptions) then
    Exit(False);  //==>


  { master-detail }
  if (cmLink in FModes) then
    if CompareRecords(FKeyBuffers[biMaster], RecBuf, FDetailFields, [loCaseInsensitive], smNone, bmNE) <> 0 then
      Exit(False);  //==>


  { range }
  if (cmRange in FModes) then
  begin
    RRS := CompareRecords(RecBuf, FKeyBuffers[biRangeStart], FRangeFields, [loCaseInsensitive], smNone, bmNE);
    RRE := CompareRecords(RecBuf, FKeyBuffers[biRangeEnd  ], FRangeFields, [loCaseInsensitive], smNone, bmNE);

    case FRangeExclusive of
      False : if not ((RRS >= 0) and (RRE <= 0)) then
                Exit(False);  //==>
      True  : if not ((RRS > 0) and (RRE < 0)) then
                Exit(False);  //==>
    end;
  end;

  { TODO: filter }
  if (cmFilter in FModes) then
  begin
    if Assigned(OnFilterRecord) then
    try
      SavedState    := SetTempState(dsFilter);

      FFilterBuffer := AllocRecordBuffer();
      CopyRecord(RecBuf, FFilterBuffer);

      Accept := True;
      OnFilterRecord(Self, Accept);
      if not Accept then
        Exit(False);  //==>
    finally
      RestoreState(SavedState);
      FreeRecordBuffer(FFilterBuffer);
      FFilterBuffer := nil;
    end;

    if Assigned(FFilterParser) and (Length(Filter) > 0) then
    begin
      if not FilterCanDisplayRecord(RecBuf) then
         Exit(False);  //==>
    end;


  end;

  Result := True;

end;

procedure TMemTable.Initialize;
var
  i             : Integer;
  FieldInfo     : TFieldInfo;
  Field         : TField;
  BufferIndex   : TKeyBufferIndex;
  DataFields    : TList;
  CalcFields    : TList;

  Ofs           : Integer;

  DataSize      : Integer;                 { plain data fields total size, i.e. fkData, fkInternalCalc - NO Calculated or Blob fields }
  CalcsSize     : Integer;                 { calc fields total size, i.e. fkCalculated, fkLookup }
  BlobsSize     : Integer;                 { blob fields total size }

  IsBlob        : Boolean;
begin

  Lock();
  try
    try
      if FInitialized then
        Finalize();

      BookmarkSize := SizeOf(Integer);

      DataFields := TList.Create();
      CalcFields := TList.Create();

      CurRecIndex               := -1;

      { separate the fields }
      for i := 0 to FFields.Count - 1 do
      begin
        FieldInfo := TFieldInfo(FFields[i]);
        Field     := FieldInfo.Field;
        IsBlob    := IsBlobFieldType(Field.DataType);

        if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
          DataFields.Add(FieldInfo)
        else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up fields }
          CalcFields.Add(FieldInfo)
        else if IsBlob then                                                                { blob fields }
          FBlobFields.Add(FieldInfo);
      end;

      { recreate the FieldInfo list according to the record buffer layout }
      FFields.Clear;                // just clear the list, do NOT free the TFieldInfo instances
      FFields.AddList(DataFields);
      FFields.AddList(CalcFields);
      FFields.AddList(FBlobFields);

      SetLength(FFieldTypes   , FFields.Count);
      SetLength(FFieldBufferSizes        , FFields.Count);
      SetLength(FOffsets      , FFields.Count);

      { process the FieldInfo list, feed the property arrays, calculate FieldInfo offsets, and FieldInfo section sizes }
      FRecBufSize := 0;
      Ofs         := 0;
      DataSize    := 0;
      CalcsSize   := 0;
      BlobsSize   := 0;

      for i := 0 to FFields.Count - 1 do
      begin
        FieldInfo := TFieldInfo(FFields[i]);
        Field     := FieldInfo.Field;
        IsBlob    := IsBlobFieldType(Field.DataType);

        FFieldTypes[i]  := Field.DataType;
        FFieldBufferSizes[i]       := GetFieldBufferSize(FieldInfo);

        FOffsets[Field.Index]  := Ofs;
        Ofs                    := Ofs + FFieldBufferSizes[i]; // + 1;

        if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
          DataSize := DataSize + FFieldBufferSizes[i] // + 1
        else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up fields }
          CalcsSize := CalcsSize + FFieldBufferSizes[i] // + 1
        else if IsBlob then                                                                { blob fields }
          BlobsSize := BlobsSize + FFieldBufferSizes[i]; // + 1;
      end;


      { NOTE: this check will raise an exception in cases where there are ftWideString look up fields.
              That is, if there are ftWideStrings look ups, (CalcsSize <> FCalcFieldsSize)  will always be True.
              And since CalcFieldsSize is not used anywhere, I think it's safe to deactivate the check }
      (*
      if (CalcsSize <> FCalcFieldsSize) and (FCalcFieldsSize <> -1) then
        raise Exception.Create('CalcSize <> CalcFieldsSize');
      *)


      { section offsets and total record buffer size }
      FCalcOfs                   := DataSize;
      FBlobOfs                   := FCalcOfs + CalcsSize ;
      FBookOfs                   := FBlobOfs + BlobsSize;
      FRecBufSize                := FBookOfs + SizeOf(TRecInfo);

      { initialize key buffers }
      for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
        FKeyBuffers[BufferIndex] := AllocRecordBuffer();

      FInitialized               := True;
    finally
      DataFields.Free;
      CalcFields.Free;
    end;
  finally
    UnLock();
  end;

end;
procedure TMemTable.Finalize;
var
  BufferIndex : TKeyBufferIndex;
begin
  Lock();
  try
    if (FInitialized) then
    begin
      { free key buffers }
      for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
        FreeMem(FKeyBuffers[BufferIndex], FRecBufSize);

      DeleteRows();

      FBlobFields.Clear;
      ClearObjectList(FFields);

      if Assigned(FFilterParser) then
         FreeAndNil(FFilterParser);

      FRecBufSize                := 0;

      FCalcOfs                   := 0;
      FBlobOfs                   := 0;
      FBookOfs                   := 0;

      FFieldTypes                := nil;
      FFieldBufferSizes          := nil;
      FOffsets                   := nil;

      FInitialized := False;
    end;
  finally
    UnLock();
  end;

end;

procedure TMemTable.DeleteRows;
var
  i      : Integer;
  RecBuf : PChar;
  FreeList : TList;
  RowStatus : TUpdateStatus;
begin
  Lock();
  try
    FRows.Clear;

    FreeList := TList.Create;
    try

      for i := 0 to FAllRows.Count - 1 do
      begin
        RecBuf := FAllRows[i];
        RowStatus := GetUpdateStatus(RecBuf);    // usUnmodified, usModified, usInserted, usDeleted

        if RowStatus in [usInserted] then
        begin
          FreeList.Add(RecBuf);
        end else begin
          PRecInfo(RecBuf + FBookOfs)^.Status := usDeleted;  // keep deleted rows in AllRows
        end;
      end;

      for i := 0 to FreeList.Count - 1 do
      begin
        RecBuf  := FreeList[i];
        FAllRows.Remove(RecBuf);
        FreeRecordBuffer(RecBuf);
      end;

      FreeList.Clear();

    finally
      FreeList.Free;
    end;


    CurRecIndex := -1;
  finally
    UnLock();
  end;

end;
procedure TMemTable.CreateDataset;
begin
  CheckInactive;

  if Fields.Count > 0 then
  begin
    FieldDefs.Clear;
    InitFieldDefsFromFields;
  end else begin
    CreateFields;
  end;

  Open();
end;
procedure TMemTable.EmptyDataSet;
begin
  if Active then
  begin
    CheckBrowseMode;
    DeleteRows();
    First;
  end;
end;

procedure TMemTable.SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
begin
  CheckBrowseMode;

  Lock();
  try
    FRangeFields.Clear;
    GetFieldInfoList(FRangeFields, RangeFieldNames);

    if FRangeFields.Count = 0 then
      FModes := FModes - [cmRange]
    else begin
      if FInitialized then
      begin
        FModes := FModes + [cmRange];

        FRangeExclusive := Exclusive;

        VariantValuesToRecordBuffer(FRangeFields, FKeyBuffers[biRangeStart], StartValues);
        VariantValuesToRecordBuffer(FRangeFields, FKeyBuffers[biRangeEnd], EndValues);
      end;
    end;

    Rebuild();
  finally
    UnLock();
  end;

  First;
  FIsInRange := True;

end;

procedure TMemTable.CancelRange;
begin
  SetRange('', Null, Null, False);
  if Active then
    First;
  FIsInRange := False;
end;

procedure TMemTable.InternalInitFieldDefs;
begin
  { nothing }
end;
procedure TMemTable.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
end;

procedure TMemTable.DoOnNewRecord;
var
  i : Integer;
begin
  FModifiedFields.Clear;

  { Default field values. NOTE: SQL expression not supported, just simple values }
  for i := 0 to Fields.Count - 1 do
    if (Fields[i].DataType <> ftLargeInt) and (Fields[i].DefaultExpression <> '') then { VCL does not fully supports LareInt Variants }
      TField(Fields[i]).Value := TField(Fields[i]).DefaultExpression;

  inherited DoOnNewRecord;
end;
procedure TMemTable.DoAfterScroll;
begin
  FModifiedFields.Clear;
  inherited DoAfterScroll;
end;

function TMemTable.GetRecordCount: Integer;
begin
  Result := FRows.Count;
end;

function TMemTable.GetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; const FieldIndex: Integer): Boolean;
var
  P          : TFieldBuffer;
begin
  Lock();
  try
    if Assigned(Buffer) then
    begin
      P          := RecBuf + FOffsets[FieldIndex];
      Result     := not IsFieldBufferNull(P, FFieldBufferSizes[FieldIndex]); // Result     := Boolean(P[0]);

      if Result then
      begin
        FillChar(Buffer^, FFieldBufferSizes[FieldIndex], 0);
        //Inc(P);
        Move(P^, Buffer^, FFieldBufferSizes[FieldIndex]);
      end;
    end;

  finally
    UnLock();
  end;
end;

procedure TMemTable.SetFieldDataInternal(RecBuf: TRecordBuffer; Buffer: PChar; const FieldIndex: Integer);
var
  P             : TFieldBuffer;
  HasData       : Boolean;
begin
  Lock();
  try
    P             := RecBuf + FOffsets[FieldIndex];
    HasData       := not IsFieldBufferNull(Buffer, FFieldBufferSizes[FieldIndex]);
    //Inc(P);

    if HasData then
    begin
      Move(Buffer^, P^, FFieldBufferSizes[FieldIndex])
    end else begin
      FillChar(P^, FFieldBufferSizes[FieldIndex], 0);
    end;
  finally
    UnLock();
  end;

end;
function TMemTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf      : PChar;
begin
  Result := False;
  RecBuf := nil;
  if GetActiveRecBuf(RecBuf) then
  begin
    if Field.FieldKind in [fkData, fkInternalCalc] then
    begin
      if (not ((State = dsBrowse) and IsEmpty)) then
         Result := GetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
    end else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Result := GetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
      (*
      Inc(RecBuf, FCursor.CalcOfs + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
      *)
    end;
  end;

end;
procedure TMemTable.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf      : PChar;
begin
  RecBuf := nil;

  if not (State in dsWriteModes) then
    DatabaseErrorFmt(SNotEditing, [Self.Name], Self);

  GetActiveRecBuf(RecBuf);

  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SReadOnlyField, [Field.DisplayName], Self);

    Field.Validate(Buffer);

    if FModifiedFields.IndexOf(Field) = -1 then
      FModifiedFields.Add(Field);

    SetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName));

  end else if (State <> dsInternalCalc) then
  begin
    SetFieldDataInternal(RecBuf, Buffer, IndexOfFieldName(Field.FieldName))
    (*
    Inc(RecBuf, FCursor.CalcOfs + Field.Offset);
    Boolean(RecBuf[0]) := LongBool(Buffer);
    if Boolean(RecBuf[0]) then
      Move(Buffer^, RecBuf[1], Field.DataSize);
    *)
  end;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, PtrInt(Field));


end;

procedure TMemTable.Lock;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    FLock.Enter;
end;

procedure TMemTable.UnLock;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    FLock.Leave();
  end;
end;
procedure TMemTable.InitializeBlobs(RecBuf: TRecordBuffer);
var
  Blob : PBlob;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      if FBlobFields.Count > 0 then
      begin
        Index := TFieldInfo(FBlobFields[0]).Field.Index;  // we need just the 1st pointer
        Blob := PBlob(RecBuf + FOffsets[Index]);
        System.Initialize(Blob^, FBlobFields.Count);
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FinalizeBlobs(RecBuf: TRecordBuffer);
var
  Blob : PBlob;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      if FBlobFields.Count > 0 then
      begin
        Index := TFieldInfo(FBlobFields[0]).Field.Index;  // we need just the 1st pointer
        Blob := PBlob(RecBuf + FOffsets[Index]);
        System.Finalize(Blob^, FBlobFields.Count);
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FreeBlobs(RecBuf: TRecordBuffer);
var
  i : Integer;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      for i := 0 to FBlobFields.Count - 1 do
      begin
        Index := TFieldInfo(FBlobFields[i]).Field.Index;
        PBlob(RecBuf + FOffsets[Index])^ := FEmptyBlob;
      end;
    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.FreeBlob(RecBuf: TRecordBuffer; FieldIndex: Integer);
begin
  Lock();
  try
    PBlob(RecBuf + FOffsets[FieldIndex])^ := FEmptyBlob;
  finally
    UnLock();
  end;
end;
procedure TMemTable.CopyBlobs(SourceRecBuf, DestRecBuf: TRecordBuffer);
var
  i : Integer;
  Index : Integer;
begin
  if FBlobFields.Count > 0 then
  begin
    Lock();
    try
      for i := 0 to FBlobFields.Count - 1 do
      begin
        Index := TFieldInfo(FBlobFields[i]).Field.Index;
        PBlob(DestRecBuf + FOffsets[Index])^ := PBlob(SourceRecBuf + FOffsets[Index])^
      end;

    finally
      UnLock();
    end;
  end;

end;
procedure TMemTable.CopyBlob(SourceRecBuf, DestRecBuf: TRecordBuffer; FieldIndex: Integer);
begin
  Lock();
  try
    PBlob(DestRecBuf + FOffsets[FieldIndex])^  := PBlob(SourceRecBuf + FOffsets[FieldIndex])^ ;
  finally
    UnLock();
  end;
end;

procedure TMemTable.CopyRecord(SourceRecBuf, DestRecBuf: TRecordBuffer);
begin
  System.Move(SourceRecBuf^, DestRecBuf^, FBlobOfs);     { copy buffer EXCEPT blobs and RecInfo }

  if FBlobFields.Count > 0 then
    CopyBlobs(SourceRecBuf, DestRecBuf);                 { copy blobs, if any }
end;

procedure TMemTable.OnFilterVariableValueEvent(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
var
  FieldInfo: TFieldInfo;
  FieldIndex: Integer;
begin

  FieldInfo := FindInfoField(Variable);
  FieldIndex := IndexOfFieldName(Variable);

  if not Assigned(FieldInfo) then
    FFilterParser.Error(Format('Field not found: %s', [Variable]));

  FieldIndex := FFields.IndexOf(FieldInfo);

  Value := GetValueFromRecBuf(TRecordBuffer(ClientTag), FieldIndex);
end;

function TMemTable.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  case State of
    dsBlockRead    ,
    dsBrowse       : if IsEmpty or (BookmarkSize = 0) then
                       RecBuf := nil
                     else
                       RecBuf := ActiveBuffer();
    dsEdit         ,
    dsInsert       : RecBuf := ActiveBuffer();

    dsCalcFields   ,
    dsInternalCalc : RecBuf := CalcBuffer;
    dsFilter       : RecBuf := FFilterBuffer;     // TODO:  dsFilter

    dsNewValue     ,
    dsOldValue     ,
    dsCurValue     : DatabaseError('GetActiveRecBuf');
    else             RecBuf := nil;

  end;

  Result := RecBuf <> nil;

end;
class function TMemTable.IsSupportedFieldType(FieldType: TFieldType): Boolean;
begin
   Result := FieldType in
   [ftString
   ,ftFixedChar
   ,ftGuid

   ,ftWideString
   ,ftFixedWideChar

   ,ftSmallint
   ,ftAutoInc
   ,ftInteger
   ,ftWord
   ,ftLargeint

   ,ftBoolean

   ,ftFloat
   ,ftCurrency

   ,ftBCD
   ,ftFMTBcd

   ,ftDate
   ,ftTime
   ,ftDateTime
   ,ftTimeStamp

   ,ftMemo
   ,ftWideMemo
   ,ftFmtMemo

   ,ftBlob
   ,ftGraphic

   ,ftOraBlob
   ,ftOraClob

   ,ftVariant
   ];
end;
class function TMemTable.IsStringFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftString
  ,ftGuid
  ,ftFixedChar
  ];
end;

class function TMemTable.IsWideStringFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftWideString
  ,ftFixedWideChar
  ];
end;

class function TMemTable.IsIntegerFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftAutoInc
  ,ftSmallint
  ,ftInteger
  ,ftWord
  ,ftLargeint
  ];
end;
class function TMemTable.IsFloatFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftFloat
  ,ftCurrency
  ];
end;

class function TMemTable.IsBCDFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftBCD
  ,ftFMTBcd
  ];
end;

class function TMemTable.IsDateTimeFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftDate
  ,ftTime
  ,ftDateTime
  ,ftTimeStamp
  ];
end;
class function TMemTable.IsVariantFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftVariant
  ];
end;
class function TMemTable.IsBlobFieldType(FieldType: TFieldType): Boolean;
begin
  Result := FieldType in
  [ftMemo
  ,ftWideMemo
  ,ftFmtMemo

  ,ftBlob
  ,ftGraphic

  ,ftOraBlob
  ,ftOraClob
  ];
end;
procedure TMemTable.AddFields();
var
  i : Integer;
  FieldInfo : TFieldInfo;
begin
  Active := False;

  for i := 0 to Fields.Count - 1 do
  begin
    FieldInfo := TFieldInfo.Create(Fields[i]);
    FFields.Add(FieldInfo);
  end;

end;
procedure TMemTable.GetFieldNames(List: TStrings);
var
  i : Integer;
begin
  for i := 0 to FFields.Count - 1 do
    List.Add(TFieldInfo(FFields[i]).Field.FieldName);
end;
function TMemTable.IndexOfField(Field: TFieldInfo): Integer;
begin
  Result := FFields.IndexOf(Field)
end;

function TMemTable.IndexOfFieldName(const FieldName: string): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FFields.Count - 1 do
    if AnsiSameText(FieldName, TFieldInfo(FFields[i]).Field.FieldName) then
    begin
      Result := i;
      Exit; //==>
    end;
end;
function TMemTable.FindInfoField(const FieldName: string): TFieldInfo;
var
  Index : Integer;
begin
  Index := IndexOfFieldName(FieldName);
  if Index = -1 then
    Result := nil
  else
    Result := TFieldInfo(FFields[Index]);
end;

function TMemTable.GetFieldInfoByName(const FieldName: string): TFieldInfo;
begin
  Result := FindInfoField(FieldName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Field not found: %s', [FieldName]);
end;
procedure TMemTable.GetFieldInfoList(List: TList; const FieldNames: string);
var
  Pos   : Integer;
  Field : TFieldInfo;
begin
  if Assigned(List) and (FieldNames <> '') then
  begin
    Pos := 1;
    while Pos <= Length(FieldNames) do
    begin
      Field := GetFieldInfoByName(ExtractFieldName(FieldNames, Pos));
      List.Add(Field);
    end;
  end;

end;
function TMemTable.GetFieldInfoList(const FieldNames: string): TList;
begin
  Result := TList.Create();
  GetFieldInfoList(Result, FieldNames);
end;
function TMemTable.GetFieldBufferSize(FieldInfo: TFieldInfo): Integer;
var
  Field: TField;
begin
  Result := 0;
  Field  := FieldInfo.Field;

  case Field.DataType of
    ftString        : Result := (Field.Size + 1) * Field.FieldDef.CharSize; // SizeOf(AnsiChar);
    ftWideString    : Result := (Field.Size + 1) * Field.FieldDef.CharSize; // SizeOf(WideChar);
    ftGuid          : Result := (Field.Size + 1) * Field.FieldDef.CharSize; // SizeOf(AnsiChar);

    ftFixedChar     : Result := (Field.Size + 1) * Field.FieldDef.CharSize; // SizeOf(AnsiChar);
    ftFixedWideChar : Result := (Field.Size + 1) * Field.FieldDef.CharSize; // SizeOf(WideChar);

    ftAutoInc       : Result := SizeOf(Integer);
    ftSmallint      : Result := SizeOf(SmallInt);
    ftInteger       : Result := SizeOf(Integer);
    ftWord          : Result := SizeOf(Word);
    ftLargeint      : Result := SizeOf(LargeInt);

    ftBoolean       : Result := SizeOf(WordBool);

    ftFloat         : Result := SizeOf(Double);
    ftCurrency      : Result := SizeOf(Double);
    ftBCD           : Result := SizeOf(TBcd);
    ftFMTBcd        : Result := SizeOf(TBcd);

    ftDate          : Result := SizeOf(Integer);
    ftTime          : Result := SizeOf(Integer);
    ftDateTime      : Result := SizeOf(TDateTime);
    ftTimeStamp     : Result := SizeOf(TTimeStamp);

    ftMemo          : Result := SizeOf(TBlob);
    ftWideMemo      : Result := SizeOf(TBlob);
    ftFmtMemo       : Result := SizeOf(TBlob);

    ftBlob          : Result := SizeOf(TBlob);
    ftGraphic       : Result := SizeOf(TBlob);

    ftOraBlob       : Result := SizeOf(TBlob);
    ftOraClob       : Result := SizeOf(TBlob);

    ftVariant       : Result := SizeOf(PVariant);
  else
    raise Exception.Create('FieldType not supported');
  end;
end;

function TMemTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  RecBuf: TRecordBuffer;
begin
  RecBuf := nil;
  GetActiveRecBuf(RecBuf);
  Result := TBlobStream.Create(Self, TBlobField(Field), RecBuf, Mode);
end;

function TMemTable.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  CheckActive();

  Result := 0;

  if Active then
  begin
    if (Bookmark1 = nil) and (Bookmark2 = nil) then
      Result := 0
    else if (Bookmark1 <> nil) and (Bookmark2 = nil) then
      Result := 1
    else if (Bookmark1 = nil) and (Bookmark2 <> nil) then
      Result := -1
    else if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1;
  end;

end;

function TMemTable.BookmarkValid(BM: TBookmark): Boolean;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive();

  Result := False;

  if Active then
  begin
    CursorPosChanged;
    RecBuf := AllocRecordBuffer();
    Result := (IndexOfBookmark(PInteger(BM)^) <> -1)  and  (GetRecord(RecBuf, gmCurrent, False) = grOK);
  end;

end;

function TMemTable.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  CheckActive;

  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemTable.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  CheckActive;

  Result := Null;

  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TMemTable.UpdateStatus: TUpdateStatus;
var
  RecBuf: TRecordBuffer;
begin
  CheckActive;

  if State = dsInternalCalc then
    Result := usUnModified
  else
  begin
    if State = dsCalcFields then
      RecBuf := CalcBuffer
    else
      RecBuf := ActiveBuffer;

    Result := PRecInfo(RecBuf + FBookOfs)^.Status;
  end;
end;
procedure TMemTable.CancelUpdates();
var
  TempList : TList;
  i        : Integer;
  RecBuf   : PChar;
begin
  if Active then
  begin
    Lock;
    try
      TempList := TList.Create;
      try
        TempList.Assign(FAllRows);
        FAllRows.Clear;

        for i := 0 to TempList.Count - 1 do
        begin
          RecBuf := TempList[i];

          if PRecInfo(RecBuf + FBookOfs)^.Status <> usDeleted then
          begin
            PRecInfo(RecBuf + FBookOfs)^.Status := usUnmodified;
            FAllRows.Add(RecBuf);
          end else
            FreeRecordBuffer(RecBuf);
        end;

      finally
        TempList.Free;
      end;

    finally
      UnLock;
    end;

    First;

  end;
end;

function TMemTable.GetBlobSize(RecBuf: PChar; FieldIndex: Integer): LongWord;
begin
  Result := GetBlobData(RecBuf, nil, FieldIndex);
end;
function TMemTable.GetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer): LongWord;
begin
  Lock();
  try
    Result := 0;
    if Boolean(PBlob(RecBuf + FOffsets[FieldIndex])^.Flag) then
    begin
      Result := Length(PBlob(RecBuf + FOffsets[FieldIndex])^.Data);
      if Assigned(Buffer) then
        Move(PChar(PBlob(RecBuf + FOffsets[FieldIndex])^.Data)^, PChar(Buffer)^, Result);
    end;
  finally
    UnLock();
  end;
end;
procedure TMemTable.SetBlobData(RecBuf, Buffer: PChar; FieldIndex: Integer; BlobSize: LongWord);
begin
  Lock();
  try
    if (BlobSize > 0) and Assigned(Buffer) then
    begin
      Boolean(PBlob(RecBuf + FOffsets[FieldIndex])^.Flag) := True;
{$IFDEF STRING_BLOBS}
      SetString(PBlob(RecBuf + FOffsets[FieldIndex])^.Data, Buffer, BlobSize);
{$ELSE}
      SetLength(PBlob(RecBuf + FOffsets[FieldIndex])^.Data, BlobSize);
      Move(Buffer^,  PChar(PBlob(RecBuf + FOffsets[FieldIndex])^.Data)^, BlobSize);
{$ENDIF}
    end else
      PBlob(RecBuf + FOffsets[FieldIndex])^ := FEmptyBlob;
  finally
    UnLock();
  end;
end;
procedure TMemTable.Sort();
var
  CurBuf : PChar;
begin
  Lock();
  try
    if Active and FInitialized and (cmSort in FModes) and (FRows.Count > 0) and (FSortOnFields.Count > 0) then
    begin
      if (CurRecIndex < 0) or (CurRecIndex >= FRows.Count) then
        CurBuf := nil
      else
        CurBuf := FRows[CurRecIndex];

      QuickSort(0, FRows.Count - 1, FRows, FSortOnFields, [], SortMode);

      CurRecIndex := FRows.IndexOf(CurBuf);

      UpdateCursorPos();
      Resync([]);
    end;
  finally
    UnLock();
  end;
end;

procedure TMemTable.Sort(FieldName: string; SortMode: TMemTableSortMode);
begin
  FSortMode := SortMode;
  SortOnFieldNames := FieldName;
end;

procedure TMemTable.NextSort(FieldName: string);
  function GetNextSortMode() : TMemTableSortMode;
  begin
    if (FSortMode = smNone) then
      Result := smAsc
    else if (FSortMode = smAsc) then
      Result := smDesc
    else
      Result := smNone;
  end;
begin
  Sort(FieldName, GetNextSortMode());
end;

procedure TMemTable.Rebuild;
var
  i      : Integer;
  CurBuf : PChar;
  RecBuf : PChar;
  Index  : Integer;
begin

  if FInitialized then
  begin
    if (CurRecIndex < 0) or (CurRecIndex >= FRows.Count) then
      CurBuf := nil
    else
      CurBuf := FRows[CurRecIndex] ;

    FRows.Clear;

    for i := 0 to FAllRows.Count - 1 do
    begin
      RecBuf := FAllRows[i];
      if CanDisplayRecord(RecBuf) then
        FRows.Add(RecBuf);
    end;

    FModes := FModes - [cmStatus];
    Sort();

    First();

    Index := FRows.IndexOf(CurBuf);
    if Index <> -1 then
    begin
      CurRecIndex := Index;
      Resync([rmExact, rmCenter]);
      DoAfterScroll;
    end;

  end;

end;

(*----------------------------------------------------------------------------
  Compare functions should return
  -1  if Item1 < Item2,
   0  if Item1 = Item2,
   1  if Item1 > Item2
 ----------------------------------------------------------------------------*)
function TMemTable.CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; Options: TLocateOptions): Integer;
var
  L, L2: Integer;

  CY, CY2 : Currency;

  DT, DT2   : TDateTime;
begin
  Result := 0;

  case FieldType of
    ftGuid         ,
    ftFixedChar    ,
    ftString       : begin
                       L  := StrLen(PChar(Data1));
                       L2 := StrLen(PChar(Data2));

                       if (loCaseInsensitive in Options) then
                       begin
                         if (loPartialKey in Options) then
                            Result := strlicomp(PChar(Data1), PChar(Data2), Min(L, L2))
                         else
                            Result := stricomp(PChar(Data1), PChar(Data2));
                       end else begin
                          if (loPartialKey in Options) then
                            Result := strlcomp(PChar(Data1), PChar(Data2), Min(L, L2))
                         else
                            Result := strcomp(PChar(Data1), PChar(Data2));
                       end;
                     end;

    ftWideString   : begin
                        L  := StrLen(PWideChar(Data1));
                        L2 := StrLen(PWideChar(Data2));

                        if (loCaseInsensitive in Options) then
                        begin
                          if (loPartialKey in Options) then
                             Result := strlicomp(PWideChar(Data1), PWideChar(Data2), Min(L, L2))
                          else
                             Result := stricomp(PWideChar(Data1), PWideChar(Data2));
                        end else begin
                           if (loPartialKey in Options) then
                             Result := strlcomp(PWideChar(Data1), PWideChar(Data2), Min(L, L2))
                          else
                             Result := strcomp(PWideChar(Data1), PWideChar(Data2));
                        end;
                     end;

    ftSmallint     : if SmallInt(Data1^) < SmallInt(Data2^) then
                       Result := -1
                     else if SmallInt(Data1^) > SmallInt(Data2^)
                       then Result := 1;

    ftInteger     ,
    ftAutoInc     :  if Longint(Data1^) < Longint(Data2^) then
                       Result := -1
                     else if Longint(Data1^) > Longint(Data2^) then
                       Result := 1;

    ftWord        : if Word(Data1^) < Word(Data2^) then
                      Result := -1
                    else if Word(Data1^) > Word(Data2^) then
                      Result := 1;

    ftBoolean     : if not WordBool(Data1^) and WordBool(Data2^) then
                      Result := -1
                    else if WordBool(Data1^) and not WordBool(Data2^) then
                      Result := 1;

    ftFloat       ,
    ftCurrency    : if Double(Data1^) < Double(Data2^) then
                      Result := -1
                    else if Double(Data1^) > Double(Data2^) then
                      Result := 1;

    ftBCD         ,
    ftFmtBCD      : begin
                     CY := 0;
                     CY2 := 0;

                     if BCDToCurr(PBcd(Data1)^, CY) and BCDToCurr(PBcd(Data2)^, CY2) then
                      if CY < CY2 then
                        Result := -1
                      else if CY > CY2 then
                        Result := 1;

                    end;
    ftDate        ,
    ftTime        ,
    ftDateTime    : begin
                      DT     := NativeToDateTime(FieldType, TDateTimeRec(Data1^));
                      DT2    := NativeToDateTime(FieldType, TDateTimeRec(Data2^));

                      { reduce the accuracy to one second }
                      Result := CompareDateTimes(DT, DT2);
                    end;

    ftTimeStamp   : begin
                      DT   := TimeStampToDateTime(TTimeStamp(Data1^));  //VarSQLTimeStampCreate(TSQLTimeStamp(Data1^));
                      DT2  := TimeStampToDateTime(TTimeStamp(Data2^));  //VarSQLTimeStampCreate(TSQLTimeStamp(Data2^));

                      Result := CompareDateTime(DT, DT2);

                    end;

    ftLargeint    : if Int64(Data1^) < Int64(Data2^) then
                      Result := -1
                    else if Int64(Data1^) > Int64(Data2^) then
                      Result := 1;

    ftVariant     : Result := 0;

  end;
end;
(*----------------------------------------------------------------------------
  Compare functions should return
  -1  if Item1 < Item2,
   0  if Item1 = Item2,
   1  if Item1 > Item2

   IndexList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.CompareRecords(const Buf1, Buf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer;
var
  Data1   : PChar;
  Data2   : PChar;
  HasData1: Boolean;
  HasData2: Boolean;
  i       : Integer;
  Field   : TField;
  Index   : Integer;

begin

  Result := 0;
  if IndexFieldList <> nil then
  begin
    for i := 0 to IndexFieldList.Count - 1 do
    begin
      Field   := TFieldInfo(IndexFieldList[i]).Field;
      Index   := Field.Index;
      Data1   := Buf1 + FOffsets[Index];

      if Data1 <> nil then
      begin
        Data2    := Buf2 + FOffsets[Index];
        if Data2 <> nil then
        begin

          HasData1 := not IsFieldBufferNull(Data1, FFieldBufferSizes[Index]);
          HasData2 := not IsFieldBufferNull(Data2, FFieldBufferSizes[Index]);

          if HasData1 and HasData2 then
          begin
            Result := CompareFields(Data1, Data2, Field.DataType, Options);
          end else if Boolean(Data1[0]) then
            Result := 1
          else if Boolean(Data2[0]) then
            Result := -1;

          if (SortMode = smDesc) then
            Result := -Result;

          case Result of
            -1 : if (BreakMode in [bmNE, bmL, bmLE]) then Break;
             1 : if (BreakMode in [bmNE, bmG, bmGE]) then Break;
          end;

        end;
      end;
    end;
  end;


  if (Result = 0) and (SortMode <> smNone) {and Assigned(RowList)} then
  begin
    if PRecInfo(Buf1 + FBookOfs)^.Bookmark  >  PRecInfo(Buf2 + FBookOfs)^.Bookmark then
      Result := 1
    else if PRecInfo(Buf1 + FBookOfs)^.Bookmark  <  PRecInfo(Buf2 + FBookOfs)^.Bookmark then
      Result := -1;

    if (SortMode = smDesc) then
       Result := -Result;
  end;
end;
procedure TMemTable.QuickSort(L, R: Integer; const RowList: TList; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode);
var
  I, J: Integer;
  P   : PChar;
begin

  repeat
    I := L;
    J := R;
    P := RowList[(L + R) shr 1];
    repeat
      while CompareRecords(RowList[I], P, IndexFieldList, Options, SortMode, bmNE) < 0 do Inc(I);
      while CompareRecords(RowList[J], P, IndexFieldList, Options, SortMode, bmNE) > 0 do Dec(J);
      if I <= J then begin
        RowList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, RowList, IndexFieldList, Options, SortMode);
    L := I;
  until I >= R;

end;

function TMemTable.GetUpdateStatus(RecBuf: TRecordBuffer): TUpdateStatus;
begin
   Result :=  PRecInfo(RecBuf + FBookOfs)^.Status;
end;

function TMemTable.GetRecordByIndex(RecBuf: TRecordBuffer; RecordIndex: Integer): Boolean;
var
  SourceRecBuf : PChar;
begin
  Lock;
  try
    if FInitialized and (RecordIndex >= 0) and (RecordIndex <= FRows.Count - 1) then
      SourceRecBuf := FRows[RecordIndex]
    else
      SourceRecBuf := nil;

    Result := Assigned(SourceRecBuf);

    if Result then
      System.Move(SourceRecBuf^, RecBuf^, FRecBufSize);
  finally
    UnLock;
  end;

end;
(*----------------------------------------------------------------------------
  IndexFieldList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean;  var RecIndex: Integer): Boolean;
var
  i                 : Integer;
  IndexList         : TList;
begin
  Lock();
  try
    Result := False;
    RecIndex := -1;

    IndexList := GetFieldInfoList(IndexFieldNames);
    try
      VariantValuesToRecordBuffer(IndexList, FKeyBuffers[biLocate], KeyValues);

      for i := 0 to FRows.Count - 1 do
        if CompareRecords(FKeyBuffers[biLocate], FRows[i], IndexList, Options, smNone, bmNE) = 0 then
        begin
          Result   := True;
          RecIndex := i;
          if SyncCursor then
            FCurRecIndex := RecIndex;
          Break;  // ==>
        end;

    finally
      IndexList.Free;
    end;

  finally
    UnLock();
  end;

end;

function TMemTable.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;
var
  RecIndex : Integer;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  CursorPosChanged;

  RecIndex := -1;
  Result := LocateRecord(KeyFields, KeyValues, Options, SyncCursor, RecIndex);
  if Result then
    Result := GetRecordByIndex(TempBuffer, RecIndex);

end;

procedure TMemTable.ClearCalcFields(RecBuf: TRecordBuffer);
var
  i : Integer;
  Field     : TField;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    Field := TFieldInfo(FFields[i]).Field;
    if not (Field.FieldKind in [fkData, fkInternalCalc]) then
    begin
      SetFieldDataInternal(RecBuf, nil, Field.Index);
    end;
  end;

end;
procedure TMemTable.VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant);
var
  i      : Integer;
  Value  : Variant;
  Index  : Integer;
  Field  : TField;
begin
  { populate the locate record buffer }
  InternalInitRecord(RecBuf);

  for i := 0 to FieldList.Count - 1 do
  begin
    Field := TFieldInfo(FieldList[i]).Field;

    if (FieldList.Count = 1) and not VarIsArray(Values) then
      Value := Values
    else
      Value := Values[i];

    if not VarIsNull(Value) then
    begin
      Index   := Field.Index;
      SetValueToRecBuf(RecBuf, Index, Value);
    end;

  end;

end;

function TMemTable.IsFieldBufferNull(FieldBuf: TFieldBuffer; Size: Integer): Boolean;
var
  i : Integer;
begin
  for i := 0 to Size - 1 do
  begin
    if PByte(FieldBuf)^ <> 0 then
    begin
      Result := False;
      Exit;
    end else begin
      Inc(FieldBuf, 1);
    end;
  end;

  Result := True;

end;

function TMemTable.GetValueFromRecBuf(RecBuf: TRecordBuffer; const FieldIndex: Integer): Variant;
var
  FieldBuf : Pointer;
begin
  FieldBuf  := RecBuf + FOffsets[FieldIndex];

  if not IsFieldBufferNull(FieldBuf, FFieldBufferSizes[FieldIndex]) then
    Result := GetValueFromFieldBuf(PChar(FieldBuf), FieldIndex)
  else
    Result := Null;
end;
procedure TMemTable.SetValueToRecBuf(RecBuf: TRecordBuffer; const FieldIndex: Integer; const Value: Variant);
var
  FieldBuf            : TFieldBuffer;
begin
  if VarIsNull(Value) then
  begin
    SetFieldDataInternal(RecBuf, nil, FieldIndex);
  end else begin
    FieldBuf     := AllocMem(FFieldBufferSizes[FieldIndex]);
    try
      SetValueToFieldBuf(FieldBuf, FieldIndex, Value);
      SetFieldDataInternal(RecBuf, FieldBuf, FieldIndex);
    finally
      FreeMem(FieldBuf);
    end;
  end;

end;
(*----------------------------------------------------------------------------
 FieldData is the Field's data buffer, null flag byte NOT included.
 This method does not perform any null flag check.
 The caller must be sure the field is not null.
 ----------------------------------------------------------------------------*)
function TMemTable.GetValueFromFieldBuf(FieldBuf: TFieldBuffer; const FieldIndex: Integer): Variant;
var
  WS                : WideString;
  CY                : Currency;

begin
  Result := Null;

  case FFieldTypes[FieldIndex] of
    ftString         ,
    ftFixedChar      ,
    ftGuid           : Result := String(PChar(FieldBuf));

    ftWideString     ,
    ftFixedWideChar  : begin
                         SetString(WS, PWideChar(PChar(FieldBuf) + SizeOf(LongWord)), LongWord(FieldBuf^) div SizeOf(WideChar));
                         Result := WS;
                       end;

    ftSmallint       : Result := SmallInt(FieldBuf^);

    ftAutoInc        ,
    ftInteger        : Result := Longint(FieldBuf^);

    ftLargeint       : Result := Int64(FieldBuf^);
    ftWord           : Result := Word(FieldBuf^);

    ftBoolean        : Result := WordBool(FieldBuf^);

    ftFloat          ,
    ftCurrency       : Result := Double(FieldBuf^);

    ftBCD            ,
    ftFMTBcd         : begin
                         CY := 0;
                         if BCDToCurr(PBcd(FieldBuf)^, CY) then
                           Result := CY;
                       end;

    ftDate           ,
    ftTime           ,
    ftDateTime       : Result  := NativeToDateTime(FFieldTypes[FieldIndex], TDateTimeRec(FieldBuf^));

    ftTimeStamp      : Result  := TimeStampToMSecs(TTimeStamp(FieldBuf^)); // VarSQLTimeStampCreate(TSQLTimeStamp(FieldData^));

    ftMemo           : ;
    ftWideMemo       : ;
    ftFmtMemo        : ;

    ftBlob           : ;
    ftGraphic        : ;

    ftOraBlob        : ;
    ftOraClob        : ;

    ftVariant        : Result := 0;
  end;

end;
(*----------------------------------------------------------------------------
 FieldData is the Field's data buffer, null flag byte NOT included.
 This method does set any null flag byte.
 The caller is responsible for setting that flag, IF FieldData comes from a record buffer.
 ----------------------------------------------------------------------------*)
procedure TMemTable.SetValueToFieldBuf(FieldBuf: TFieldBuffer; const FieldIndex: Integer; Value: Variant);
var
  Data : TVarData;

  BDC    : TBcd;
  P      : Pointer;

  DTR     : TDateTimeRec;
  DT      : TDateTime;
  //TS      : TSQLTimeStamp;
begin
  Data := TVarData(Value);

  case FFieldTypes[FieldIndex] of
    ftString                 ,
    ftFixedChar              ,
    ftGuid                   : if not VarIsNull(Value) then
                                 StrCopy(PChar(FieldBuf), PChar(String(Value)));

    ftFixedWideChar          ,
    ftWideString             : if not VarIsNull(Value) then
                                 WideToBuffer(Value, FieldBuf);

    ftSmallint               : if Data.VType = varByte then
                                 SmallInt(FieldBuf^) := Byte(Value)
                               else
                                 SmallInt(FieldBuf^) := SmallInt(Value) ;

    ftAutoInc                ,
    ftInteger                : Integer(FieldBuf^) := Integer(Value) ;

    ftLargeint               : Int64(FieldBuf^) := Int64(Value) ;

    ftWord                   : if Data.VType = varByte then
                                 Word(FieldBuf^) := Byte(Value)
                               else
                                 Word(FieldBuf^) := Word(Value) ;

    ftBoolean                : WordBool(FieldBuf^) := WordBool(Value);

    ftFloat                  ,
    ftCurrency               : if Data.VType = varDouble then
                                 Double(FieldBuf^) := Double(Value)
                               else
                                 Double(FieldBuf^) := Value;

    ftFMTBcd                 ,
    ftBCD                    : begin
                                 BDC.Precision := 0;
                                 BDC.SignSpecialPlaces := 0;
                                 CurrToBCD(Value, BDC, 32, Fields[FieldIndex].Size);
                                 P := @BDC;
                                 Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                               end;

    ftDate                   ,
    ftTime                   ,
    ftDateTime               : begin
                                 DTR := DateTimeToNative(FFieldTypes[FieldIndex], VarToDateTime(Value));
                                 P   := @DTR;
                                 Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                               end;

    ftTimeStamp              : begin
                                 {
                                   not directly VarToSQLTimeStamp(Value)
                                   because if Value is a string
                                   the VarToSQLTimeStamp creates a never destroyed TSQLTimeStampData
                                   and that leads to memory leak in Delphi 7
                                 }
                                 DT  := VarToDateTime(Value); //VarToSQLTimeStamp(VarToDateTime(Value));
                                 P   := @DT;
                                 Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                                 Value := Null;
                               end;

    ftVariant                : Variant(FieldBuf^) := Value;
  else
    raise Exception.CreateFmt('Unsupported field type (%s) in field %s', [FieldTypeNames[FFieldTypes[FieldIndex]], Fields[FieldIndex].DisplayLabel]);
  end;

end;
function TMemTable.GetMasterDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;
procedure TMemTable.SetMasterDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    if Value.IsLinkedTo(Self) then
      DatabaseError(SErrCircularDataSourceReferenceNotAllowed);

  FMasterLink.DataSource := Value;
end;
function TMemTable.GetMasterFieldNames: string;
begin
  Result := FMasterLink.FieldNames;
end;
procedure TMemTable.SetMasterFieldNames(Value: string);
begin
  if FMasterLink.FieldNames <> Value then
    FMasterLink.FieldNames := Value;
end;
procedure TMemTable.SetDetailFieldNames(Value: string);
begin
  if FDetailFieldNames <> Value then
  begin
    FDetailFieldNames := Value;
    LoadDetailFieldList();
  end;
end;



procedure TMemTable.SetSortMode(Value: TMemTableSortMode);
begin
  FSortMode := Value;

  if FSortMode <> smNone then
     FModes := FModes + [cmSort]
  else
     FModes := FModes - [cmSort];

  Sort();

end;
procedure TMemTable.SetSortOnFieldNames(Value: string);
begin
  FSortOnFieldNames := Value;

  LoadIndexFieldList();

  if (FSortOnFields.Count > 0) then
  begin
    FModes := FModes + [cmSort];
  end else begin
    FModes := FModes - [cmSort];
  end;

  Sort();
end;
procedure TMemTable.LoadIndexFieldList();
begin
  if Active or (FieldCount > 0) then
  begin

    Lock();
    try
      FSortOnFields.Clear;
      GetFieldInfoList(FSortOnFields, FSortOnFieldNames);
    finally
      UnLock();
    end;
  end;
end;
function TMemTable.GetStatusFilter: TUpdateStatusSet;
begin
  Result := FStatusOptions
end;
procedure TMemTable.SetStatusFilter(Value: TUpdateStatusSet);
begin
  if Value <> StatusFilter then
  begin
    if Value = [] then
      Value := [usModified, usInserted, usUnmodified] ;

    if FStatusOptions <> Value then
    begin
      FStatusOptions := Value;
      FModes := FModes + [cmStatus];
      Rebuild();
      if (FStatusOptions = [usModified, usInserted, usUnmodified]) then
        FModes := FModes - [cmStatus];
    end;

    if Active then
    begin
      Last;
      First;
    end;
  end;
end;

procedure TMemTable.OnMasterLinkChanged(Sender: TObject);

  function VarEquals(const V1, V2: Variant): Boolean;
  begin
    Result := False;
    try
      Result := VarSameValue(V1, V2);
    except
    end;
  end;

  function FieldValuesChanged(FieldList, FieldInfoList: TList): Boolean;
  var
    i : Integer;
  begin
    Result := False;
    if (FieldList.Count > 0) and (FieldList.Count = FieldInfoList.Count) then
      for i := 0 to FieldList.Count - 1 do

        if not VarEquals(TField(FieldList[i]).Value, TFieldInfo(FieldInfoList[i]).Field.Value) then
        begin
          Result := True;
          Exit; //==>
        end;
  end;

begin
  CheckBrowseMode;

  if Assigned(FMasterLink.DataSet)
    and (FMasterLink.Fields.Count > 0)
    and (not (FMasterLink.DataSet.State in [dsEdit, dsInsert]))
    and (FMasterLink.Fields.Count > 0)
    and (FMasterLink.Fields.Count = FDetailFields.Count) then
  begin
    if FieldValuesChanged(FMasterLink.Fields, FDetailFields) then
    begin
      SetLink(True);
      First;
    end;
  end;

end;
procedure TMemTable.OnMasterLinkDisabled(Sender: TObject);
begin
  SetLink(False);
  if Active then
    First();
end;
procedure TMemTable.LoadDetailFieldList();
begin
  if Active or (FieldCount > 0) then
  begin
    FDetailFields.Clear;
    GetFieldInfoList(FDetailFields, FDetailFieldNames);
  end;
end;


procedure TMemTable.SetLink(Value: Boolean);
var
  i            : Integer;
  FieldIndex   : Integer;
  Buf          : Pointer;
  MasterFields : TList;    // MasterFields is a TList containing TField objects of the master TDataLink
begin

  Lock();
  try

    if Value then          // =========== Link
    begin
      MasterFields := TList.Create();
      try
        MasterFields.Assign(FMasterLink.Fields, laOr);

        { populate the master record buffer }
        FillChar(FKeyBuffers[biMaster]^, FRecBufSize, 0);

        for i := 0 to FDetailFields.Count - 1 do
          if not TField(MasterFields[i]).IsNull then
          begin
            FieldIndex   := TFieldInfo(FDetailFields[i]).Field.Index;
            Buf          := AllocMem(FFieldBufferSizes[FieldIndex]);
            try
              TField(MasterFields[i]).GetData(Buf);                             // get the master field buffer
              SetFieldDataInternal(FKeyBuffers[biMaster], Buf, FieldIndex);     // put the master field buffer if FKeyBuffers
            finally
              FreeMem(Buf);
            end;
          end;
      finally
        MasterFields.Free;
      end;
      FModes := FModes + [cmLink];
    end else begin      // =========== Un-Link
      FModes := FModes - [cmLink];
    end;

    Rebuild();
  finally
    UnLock();
  end;

end;




class function TMemTable.DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Result.Date := TimeStamp.Date;
    ftTime: Result.Time := TimeStamp.Time;
  else
    Result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;

class function TMemTable.NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
var
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Data.Date;
      end;
    ftTime:
      begin
        TimeStamp.Time := Data.Time;
        TimeStamp.Date := DateDelta;
      end;
  else
    try
      TimeStamp := DateTimeToTimeStamp(Data.DateTime);
    except
      TimeStamp.Time := 0;
      TimeStamp.Date := 0;
    end;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;

class function TMemTable.CompareDateTimes(const A, B: TDateTime): Integer;
begin
  Result := CompareDateTime(A, B);
end;

class function TMemTable.BufferToWide(Buffer: Pointer): WideString;
var
  Len  : LongWord;
begin
  Result := '';
  Len           := PLongWord(Buffer)^;
  if Len <> 0 then
  begin
    SetLength(Result, Len div SizeOf(WideChar));
    Move(Pointer(PChar(Buffer) + SizeOf(LongWord))^, Pointer(Result)^, Len);
  end;
end;

class procedure TMemTable.WideToBuffer(WS: WideString; Buffer: Pointer);
var
  Len : LongWord;
  Source : PChar;
begin
{$WARNINGS OFF}
  if Length(WS) > 0 then
  begin
    Source        := PChar(WS) - SizeOf(LongWord);
    Len           := PLongWord(Source)^;
    Move(Source^, Buffer^, Len + SizeOf(LongWord));
  end;
{$WARNINGS ON}
end;

class procedure TMemTable.GetFloatFormatAndDigits(CurrencyFormat: Boolean; var FloatFormat: TFloatFormat; var Digits: Integer);
begin
  if CurrencyFormat then
  begin
    FloatFormat := ffCurrency;
    Digits      := DefaultFormatSettings.CurrencyFormat;
  end else begin
    FloatFormat := ffGeneral;
    Digits      := 0;
  end;
end;
class procedure TMemTable.ClearObjectList(List: TList);
begin
  while (List.Count > 0) do
  begin
    try
      TObject(List[List.Count - 1]).Free;
    except
    end;
    List.Delete(List.Count - 1);
  end;
end;

class function  TMemTable.Min(const A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

class function  TMemTable.Max(const A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;





end.
