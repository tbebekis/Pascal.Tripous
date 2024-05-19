unit Tripous.MemTable;

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes
 ,SysUtils
 ,DateUtils
 ,Variants
 ,DB
 ,FmtBCD

 ,Graphics
 ,base64
 ,TypInfo
 ,laz2_DOM

 ,Tripous.FilterParser
 ;

type

  { TFieldDefsStringFieldHelper }
  TFieldDefsStringFieldHelper = class helper for TFieldDefs
    { For the Size of an ftString field to be respected, in characters, even for non-English characters,
      the CodePage should be CP_UTF8. Not CP_ACP.

      SEE: https://forum.lazarus.freepascal.org/index.php/topic,6950.msg516328.html#msg516328 }
    function AddString(FieldName: string; Size: Integer; Required: Boolean = False; ReadOnly: Boolean = False; CodePage: TSystemCodePage = CP_UTF8): TFieldDef;
  end;

  { TDatasetHelper }
  TDatasetHelper = class helper for TDataset
    function  AddField(const FieldName: string; FieldType: TFieldType; Size: Integer = 0; Precision: Integer = -1; Required: Boolean = False; ReadOnly: Boolean = False; CodePage: TSystemCodePage = CP_UTF8) : TField;
    function  AddLookUpField(const FieldName, KeyFields: string; LookupDataSet: TDataSet; const LookupKeyFields, LookupResultField: string): TField;
    function  AddCalcField(const FieldName: string; FieldType: TFieldType; FieldSize: Integer = 0): TField;
  end;

  TCreateDatasetProc = procedure of object;

  { XmlPersistor }
  XmlPersistor = class
    class function  CreateXmlDoc(RootName: string; aEncoding: string): TXMLDocument;

    class function  ToXmlDoc(Table: TDataset; SchemaOnly: Boolean = False): TXMLDocument;
    class function  ToXmlText(Table: TDataset; SchemaOnly: Boolean = False): string;
    class procedure SaveToXmlFile(Table: TDataset; const FileName: string; SchemaOnly: Boolean = False);
    class procedure SaveToXmlStream(Table: TDataset; Stream: TStream; SchemaOnly: Boolean = False);

    class procedure FromXmlDoc(Table: TDataset; Doc: TXMLDocument; SchemaOnly: Boolean = False);
    class procedure FromXmlText(Table: TDataset; XmlText: string; SchemaOnly: Boolean = False);
    class procedure LoadFromXmlFile(Table: TDataset; const FileName: string; SchemaOnly: Boolean = False);
    class procedure LoadFromXmlStream(Table: TDataset; Stream: TStream; SchemaOnly: Boolean = False);
  end;

  (*============================================================================
                             Record buffer layout
   -----------------------------------------------------------------------------

   +-----------------+-------------------+-------------------+-----------------+
   |                 |                   |                   |                 |
   |   Data Fields   |    Calc Fields    |    Blob fields    |    TRecInfo     |
   |                 |                   |                   |                 |
   +-----------------+-------------------+-------------------+-----------------+
   0                 CalcOfs             BlobOfs             BookOfs


   ----------------------------------------------------------------------------
   Data fields         : fkData and fkInternalCalc fixed length fields
   Calculated fields   : fkLookup and fkCalculated fields
   Blob fields         : pointers to the actual blobs
   ----------------------------------------------------------------------------
   Field buffer layout : byte  0         is the Null Flag Byte (0 = null, else not null)
                         bytes 1..n      field data
   ----------------------------------------------------------------------------
   Blob buffer layout  : see TBlob below, which is used in accessing blobs.
   ----------------------------------------------------------------------------
   TRecInfo layout     : see below
   ============================================================================*)

  TMemTableSortMode  = (smNone, smAsc, smDesc);

  // PtrUInt is an unsigned integer type which has always the same size as a pointer.
  // When using integers which will be cast to pointers and vice versa, use this type
  // The following type is used as bookmark data
  TIntBM = PtrUInt;
  PIntBM = ^TIntBM;


  { TFieldInfo }
  TFieldInfo = class(TObject)
  protected
    FField             : TField;
    FIsAutoInc         : Boolean;
    FIsBlob            : Boolean;
    FIndex             : Integer;
    FFieldName         : string;
  public
    constructor Create(Field: TField; aIndex: Integer);

    property Field        : TField read FField;
    property FieldName    : string read FFieldName;
    property IsAutoInc    : Boolean read FIsAutoInc;
    property IsBlob       : Boolean read FIsBlob;
    property Index        : Integer read FIndex;
  end;

  { TMemTable }
  TMemTable = class(TDataSet)
  protected
    type
      PRecInfo = ^TRecInfo;
      TRecInfo = record
        Id             : LongWord;        // unique record Id
        Bookmark       : TIntBM;          // the BookMark
        BookmarkFlag   : TBookmarkFlag;   // = (bfCurrent, bfBOF, bfEOF, bfInserted)
        Status         : TUpdateStatus;   // = (usUnmodified, usModified, usInserted, usDeleted)
      end;

      PBlob = ^TBlob;
      TBlob = packed record
        Size    : PtrUInt;
        Data    : Pointer;
      end;

      TKeyBufferIndex = (biMaster
                        ,biRangeStart
                        ,biRangeEnd
                        ,biLocate
                        );


      { used in comparing fields in order to break the comparison loops }
      TBreakMode    = ( bmG
                       ,bmGE
                       ,bmNE
                       ,bmL
                       ,bmLE
                       );

      { the cursor may be in one or more modes }
      TCursorMode = ( cmStatus
                      ,cmLink
                      ,cmRange
                      ,cmSort
                      ,cmFilter
                     );

      TCursorModes = set of TCursorMode;

  protected
    FTableName                 : string;

    FAllRows                   : TList;                   { all rows - owned, all record buffers. Buffers must be freed. }
    FRows                      : TList;                   { current rows - record buffers passing filters, ranges, etc. }

    FFields                    : TList;                   { all TFieldInfo fields - owned. TFieldInfo instances must be freed. }
    FDetailFields              : TList;                   { TFieldInfo list }
    FSortOnFields              : TList;                   { TFieldInfo list }
    FRangeFields               : TList;                   { TFieldInfo list }
    FModifiedFields            : TList;                   { TField list }

    FBlobCount                 : LongWord;                { number of Blob fields }

    { record buffer total size }
    FRecBufSize                : LongWord;                { record buffer total size - all fields + SizeOf(TRecInfo)  }

    { offsets }
    FCalcOfs                   : LongWord;
    FBlobOfs                   : LongWord;
    FBookOfs                   : LongWord;

    { field attribute arrays }
    FFieldTypes                : array of TFieldType;
    FFieldBufferSizes          : array of LongWord;       { data size of field. Null flag is NOT included. }
    FOffsets                   : array of LongWord;       { offset of a field data. For non-blob fields, the first byte is the Null Flag  byte. Blob fields have a Size property. }

    FCurRecIndex               : LongInt;                 { the current record index in the FRows list }
    FLastBookmark              : TIntBM;                  { an auto inc number unique for each record, stored in TRecInfo.Bookmark }

    FModes                     : TCursorModes;
    FKeyBuffers                : array[TKeyBufferIndex] of TRecordBuffer;

    { master/detail }
    FMasterLink                : TMasterDataLink;
    FDetailFieldNames          : string;

    { sort }
    FSortMode                  : TMemTableSortMode;
    FSortOnFieldNames          : string;

    { status filter }
    FStatusOptions             : TUpdateStatusSet;

    { range }
    FIsInRange                 : Boolean;
    FRangeExclusive            : Boolean;

    { filter }
    FFilterParser              : TFilterParser;
    FFilterBuffer              : TRecordBuffer;
    FIsFilterActive            : Boolean;

    { miscs }
    FInitialized               : Boolean;
    FLastRecId                 : LongWord;
    FEncoding                  : string;                 { defaults to utf-8 }
    FAutoIncEnabled            : Boolean;


    { getters/setters }
    function  GetMasterDataSource: TDataSource;
    procedure SetMasterDataSource(Value: TDataSource);
    function  GetMasterFieldNames: string;
    procedure SetMasterFieldNames(Value: string);
    procedure SetSortMode(Value: TMemTableSortMode);
    procedure SetSortOnFieldNames(Value: string);
    function  GetStatusFilter: TUpdateStatusSet;
    procedure SetStatusFilter(Value: TUpdateStatusSet);
    procedure SetIsFilterActive(Value: Boolean);
    function  GetEncoding(): string;

    { initialization }
    procedure Initialize;
    procedure Finalize;
    procedure DeleteRows;

    { pointer arithmetic }
    function  GetRecInfo(RecBuf: Pointer): PRecInfo;
    function  GetFieldPtr(RecBuf: Pointer; FieldIndex: Integer): PByte;
    function  GetBlobPtr(RecBuf: Pointer; FieldIndex: Integer): PBlob;

    { null flag }
    function  IsFieldNull(RecBuf: Pointer; FieldIndex: Integer): Boolean;
    procedure SetFieldNullFlag(RecBuf: Pointer; FieldIndex: Integer; ToNull: Boolean);

    { read/write/copy field }
    function  GetFieldDataInternal(RecBuf: Pointer; Buffer: Pointer; const FieldIndex: Integer): Boolean;
    procedure SetFieldDataInternal(RecBuf: Pointer; Buffer: Pointer; const FieldIndex: Integer);

    procedure CopyFieldData(SourceRecBuf, DestRecBuf: Pointer; const FieldIndex: Integer); overload;
    procedure CopyFieldData(SourceRecBuf, DestRecBuf: Pointer; FieldInfo: TFieldInfo); overload;

    { blobs }
    function  GetBlobData(RecBuf, Buffer: Pointer; FieldIndex: Integer): PtrUInt;
    procedure SetBlobData(RecBuf, Buffer: Pointer; FieldIndex: Integer; BlobSize: PtrUInt);
    function  GetBlobSize(RecBuf: Pointer; FieldIndex: Integer): PtrUInt;

    procedure FreeBlobs(RecBuf: Pointer);
    procedure FreeBlob(Blob: PBlob);

    { record }
    function  CanDisplayRecord(RecBuf: Pointer): Boolean;
    procedure CopyRecord(SourceRecBuf, DestRecBuf: Pointer; CopyRecInfoToo: Boolean);
    function  GetActiveRecBuf(var RecBuf: Pointer): Boolean;

    { bookmark }
    function  GoToBookmarkInternal(BM: TIntBM): Boolean;
    function  GetBookmarkInternal(RecBuf: Pointer): TIntBM;
    function  IndexOfBookmark(BM: TIntBM): Integer;

    { master-detail }
    procedure OnMasterLinkChanged(Sender: TObject);
    procedure OnMasterLinkDisabled(Sender: TObject);
    procedure SetLink(Value: Boolean);

    { operations }
    procedure Rebuild();
    procedure Sort(); overload;

    { comparing-sorting }
    function  CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; Options: TLocateOptions): Integer;
    function  CompareRecords(const RecBuf1, RecBuf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer;
    procedure QuickSort(L, R: Integer; const RowList: TList; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode);

    { field get/set value as Variant }
    function  GetValueFromRecBuf(RecBuf: Pointer; const FieldIndex: Integer): Variant;
    procedure SetValueToRecBuf(RecBuf: Pointer; const FieldIndex: Integer; const Value: Variant);
    function  GetValueFromFieldBuf(FieldBuf: Pointer; const FieldIndex: Integer): Variant;
    procedure SetValueToFieldBuf(FieldBuf: Pointer; const FieldIndex: Integer; Value: Variant);

    { record miscs }
    function  GetRecordByIndex(RecBuf: Pointer; RecordIndex: Integer): Boolean;
    function  LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; var RecIndex: Integer): Boolean; overload;
    function  LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;  overload;

    { filter }
    procedure OnFilterVariableValueEvent(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);

    { TFieldInfo related }
    procedure AddFieldInfos();

    procedure GetFieldNames(List: TStrings);
    function  GetFieldInfo(Field: TField): TFieldInfo; overload;
    function  GetFieldInfo(const FieldIndex: Integer): TFieldInfo; overload;
    function  GetFieldInfo(const FieldName: string): TFieldInfo;  overload;

    procedure GetFieldInfoList(List: TList; const FieldNames: string); overload;
    function  GetFieldInfoList(const FieldNames: string): TList; overload;

    { miscs }
    procedure LoadSortOnFieldList();
    procedure VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant);
  protected
    {== TDataset overrides ==}

    { record buffer }
    function  AllocRecordBuffer(): TRecordBuffer; override;
    procedure FreeRecordBuffer(var RecBuf: TRecordBuffer); override;
    function  GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize(): Word; override;

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
    procedure InternalPost; override;
    procedure InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean); override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;

    { open/close }
    function  IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

    { filter - overrides }
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    { filter - own }
    function  CanActivateFilter(): Boolean;
    property  IsFilterActive: Boolean read FIsFilterActive write SetIsFilterActive;
    function  FilterCanDisplayRecord(RecBuf: Pointer): Boolean;

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

    function  BookmarkValid(BM: TBookmark): Boolean; override;
    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    function  Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function  Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function  UpdateStatus: TUpdateStatus; override;

    { additional }
    procedure CreateDataset();
    procedure EmptyDataSet;
    procedure CancelUpdates();

    procedure Sort(FieldNames: string; SortMode: TMemTableSortMode = smAsc);  overload;
    procedure NextSort(FieldNames: string);
    function  GetNextSortMode(): TMemTableSortMode;

    procedure SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
    procedure CancelRange;

    { xml }
    function  ToXmlText(SchemaOnly: Boolean = False): string;
    procedure SaveToXmlFile(const FileName: string; SchemaOnly: Boolean = False);
    procedure SaveToXmlStream(Stream: TStream; SchemaOnly: Boolean = False);

    procedure FromXmlText(XmlText: string; SchemaOnly: Boolean = False);
    procedure LoadFromXmlFile(const FileName: string; SchemaOnly: Boolean = False);
    procedure LoadFromXmlStream(Stream: TStream; SchemaOnly: Boolean = False);

    { supported field types }
    class function IsSupportedFieldType(FieldType: TFieldType): Boolean;

    class function IsStringFieldType(FieldType: TFieldType): Boolean;
    class function IsWideStringFieldType(FieldType: TFieldType): Boolean;
    class function IsIntegerFieldType(FieldType: TFieldType): Boolean;
    class function IsFloatFieldType(FieldType: TFieldType): Boolean;
    class function IsBCDFieldType(FieldType: TFieldType): Boolean;
    class function IsDateTimeFieldType(FieldType: TFieldType): Boolean;
    class function IsBlobFieldType(FieldType: TFieldType): Boolean;

    { helpers }
    class procedure CopyMem(Source: Pointer; Dest: Pointer; Length: PtrUInt);
    class procedure ClearObjectList(List: TList);
    class procedure AddList(Source, Dest: TList);
    class function  DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
    class function  NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
    class function  CompareDateTimes(const A, B: TDateTime): Integer;
    class function  BufferToWide(Buffer: Pointer): WideString;
    class procedure WideToBuffer(WS: WideString; Buffer: Pointer);
    class procedure GetFloatFormatAndDigits(CurrencyFormat: Boolean; var FloatFormat: TFloatFormat; var Digits: Integer);
    class function  Min(const A, B: Integer): Integer;
    class function  Max(const A, B: Integer): Integer;
    class function  NewGuid(UseBrackets: Boolean = True): string;

    { properties }
    property MasterSource      : TDataSource read GetMasterDataSource write SetMasterDataSource;
    property MasterFieldNames  : string read GetMasterFieldNames write SetMasterFieldNames;       { ; delimited list }
    property DetailFieldNames  : string read FDetailFieldNames  write FDetailFieldNames;          { ; delimited list }
    property SortOnFieldNames  : string read FSortOnFieldNames write SetSortOnFieldNames;         { ; delimited list }
    property SortMode          : TMemTableSortMode read FSortMode write SetSortMode;
    property StatusFilter      : TUpdateStatusSet read GetStatusFilter write SetStatusFilter;
    property Encoding          : string read GetEncoding write FEncoding;
    property AutoIncEnabled    : Boolean read FAutoIncEnabled write FAutoIncEnabled;
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
 ,laz2_XMLRead
 ,laz2_XMLWrite
 ;

resourcestring
  SUnsupportedFieldType    = 'Fieldtype %s is not supported';
  SReadOnlyField           = 'Field %s cannot be modified, it is read-only.';
  SSchemaNotDefined        = 'Cannot open dataset. No FieldDefs and no Fields defined.';


const
  NULL_STR   = '##null##';

  InvariantFormatSettings : TFormatSettings = (
      CurrencyFormat: 1;
      NegCurrFormat: 5;
      ThousandSeparator: ',';
      DecimalSeparator: '.';
      CurrencyDecimals: 2;
      DateSeparator: '-';
      TimeSeparator: ':';
      ListSeparator: ',';
      CurrencyString: '$';
      ShortDateFormat: 'yyyy-mm-dd';          // 'd/m/y';
      LongDateFormat: 'yyyy-mm-dd';           // 'dd" "mmmm" "yyyy';
      TimeAMString: 'AM';
      TimePMString: 'PM';
      ShortTimeFormat: 'hh:nn';
      LongTimeFormat: 'hh:nn:ss';
      ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                        'Jul','Aug','Sep','Oct','Nov','Dec');
      LongMonthNames: ('January','February','March','April','May','June',
                       'July','August','September','October','November','December');
      ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
      LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
      TwoDigitYearCenturyWindow: 50;
    );


function HasProperty(Instance: TObject; const PropertyName: string): Boolean;
begin
  Result := GetPropInfo(Instance.ClassInfo, PropertyName) <> nil;
end;


{ TFieldDefsStringFieldHelper }

function TFieldDefsStringFieldHelper.AddString(FieldName: string; Size: Integer; Required: Boolean; ReadOnly: Boolean; CodePage: TSystemCodePage): TFieldDef;
begin
  Result := Add(FieldName, ftString, Size, 0, Required, ReadOnly, Count + 1, CodePage);
end;

function TDatasetHelper.AddField(const FieldName: string; FieldType: TFieldType; Size: Integer; Precision: Integer; Required, ReadOnly: Boolean; CodePage: TSystemCodePage): TField;
var
  Field: TField;
  FieldClass : TFieldClass;
begin

  { NOTE: No way to set CodePage to UTF8 when creating a TField }
  if (FieldType = ftString) and (CodePage = CP_UTF8) then
     FieldClass := TWideStringField
  else
    FieldClass := DB.DefaultFieldClasses[FieldType];

  Field := FieldClass.Create(Self);
  Field.DataSet         := Self;
  Field.FieldName       := FieldName;
  Field.DisplayLabel    := FieldName;
  Field.Size            := Size;
  Field.Required        := Required;
  Field.ReadOnly        := ReadOnly;

  { NOTE: No way to set CodePage to UTF8 when creating a TField }
  {
  if (Field is TStringField) then
    TStringField(Field).CodePage := CodePage
  else if (Field is TMemoField) then
    TMemoField(Field).CodePage := CodePage
  else
  }

  if (Field is TFloatField) then
    TFloatField(Field).Precision := Precision
  else if (Field is TBCDField) then
    TBCDField(Field).Precision := Precision
  else if (Field is TFmtBCDField) then
    TFmtBCDField(Field).Precision := Precision;

  Result := Field;
end;

{ TDatasetHelper }
function  TDatasetHelper.AddLookUpField(const FieldName, KeyFields: string; LookupDataSet: TDataSet; const LookupKeyFields, LookupResultField: string): TField;
var
  ResultField : TField;
begin
  ResultField := LookupDataSet.FindField(LookupResultField);

  if not Assigned(ResultField) then
    raise Exception.CreateFmt('Lookup Result field not found in Lookup dataset: %s', [LookupResultField]);

  if not ((ResultField is TStringField) or (ResultField is TWideStringField)) then
    raise Exception.Create('Lookup Result field must be either a TStringField or a TWideStringField field');

  if (Self.FieldCount = 0) then
    raise Exception.Create('Cannont create a Lookup field. No fields in the dataset');

  Result := TWideStringField.Create(Self);
  Result.FieldName := FieldName;
  Result.KeyFields := KeyFields;
  Result.LookupDataSet := LookupDataSet;
  Result.LookupKeyFields := LookupKeyFields;
  Result.LookupResultField := LookupResultField;
  Result.FieldKind := fkLookup;
  Result.Dataset := Self;
  Result.Size := ResultField.Size;
end;

function TDatasetHelper.AddCalcField(const FieldName: string; FieldType: TFieldType; FieldSize: Integer): TField;
begin
  Result := DefaultFieldClasses[FieldType].Create(Self);
  Result.FieldName := FieldName;
  Result.FieldKind := fkCalculated;
  Result.Dataset := Self;
  Result.Size := FieldSize;
end;






{ XmlPersistor }
class function XmlPersistor.CreateXmlDoc(RootName: string; aEncoding: string): TXMLDocument;
begin
  Result := TXMLDocument.Create;

  if aEncoding = '' then
    aEncoding := 'utf-8';

  if RootName = '' then
    RootName := 'root';

  Result.AppendChild(Result.CreateElement(RootName));

  Result.XMLVersion := '1.0';
  Result.Encoding   := aEncoding;
end;

class function XmlPersistor.ToXmlDoc(Table: TDataset; SchemaOnly: Boolean): TXMLDocument;
  {---------------------------------------------------}
  function StrToXml(const S: string): string;
  const
    SpecChars = ['<', '>', '"', '&', #10, #13];
  var
    i: Integer;

    procedure ReplaceChars(var S: string; i: Integer);
    begin
      System.Insert('#' + IntToStr(Ord(S[i])) + ';', S, i + 1);
      S[i] := '&';
    end;

  begin
    Result := S;
    for i := Length(S) downto 1 do
      if S[i] in SpecChars then
        ReplaceChars(Result, i);
  end;
  {---------------------------------------------------}
  function AddNode(Parent: TDOMNode; ChildName: string): TDOMElement;
  begin
    Result := Parent.OwnerDocument.CreateElement(ChildName);
    Parent.AppendChild(Result);
  end;
  {---------------------------------------------------}
  procedure SetAttr(const Node: TDOMNode; AttrName: string; Value: Variant);
  var
    sValue: string;
  begin
     if (Node is TDOMElement) then
     begin
       if (VarIsNull(Value)) then
          sValue := ''
       else
         sValue := VarToStr(Value);

       TDOMElement(Node)[AttrName] := sValue;
     end;
  end;
  {---------------------------------------------------}
  function StreamToBase64(Input: TStream): string;
  var
    Output: TStringStream;
    Encoder : TBase64EncodingStream;
  begin
    Result := '';

    if (Input.Size > 0) then
    begin
      Input.Position := 0;

      Output := TStringStream.Create();
      try
        Encoder := TBase64EncodingStream.Create(Output);
        try
          Encoder.CopyFrom(Input, Input.Size);
          Result := Output.DataString;
        finally
          Encoder.Free;
        end;
      finally
         Output.Free;
      end;
    end;
  end;
  {---------------------------------------------------}
var
  Root      : TDOMElement;
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  i         : Integer;
  Field     : TField;
  TagName   : string;
  BM        : TBytes;
  MS        : TMemoryStream;
  V         : Variant;
  FS        : TFormatSettings;
begin
  FS     := InvariantFormatSettings;

  Result := CreateXmlDoc('datapacket', 'utf-8');
  Root   := Result.DocumentElement;

  if HasProperty(Table, 'TableName') then
     SetAttr(Root, 'TableName', StrToXml(GetStrProp(Table, 'TableName')));

  // schema ======================================================
  Node := AddNode(Root, 'schema');

  for i := 0 to Table.FieldCount - 1 do
  begin
    Field   := Table.Fields[i];
    if Field.FieldKind = fkData then
    begin
      SubNode := AddNode(Node, 'c' + IntToStr(i));
      SubNode['name'] := Field.FieldName;

      case Field.DataType of
        ftString        ,
        ftFixedChar     : SubNode['type']  := 'string';
        ftGuid          : SubNode['guid']  := 'string';

        ftWideString    ,
        ftFixedWideChar : SubNode['type']  := 'widestring';

        ftSmallint      ,
        ftInteger       ,
        ftWord          ,
        ftLargeint      : SubNode['type'] := 'integer';
        ftAutoInc       : SubNode['type'] := 'autoinc';

        ftBoolean       : SubNode['type'] := 'boolean';

        ftFloat         ,
        ftBCD           ,
        ftFMTBcd        : SubNode['type'] := 'float';
        ftCurrency      : SubNode['type'] := 'money';

        ftDate          : SubNode['type'] := 'date';
        ftTime          : SubNode['type'] := 'time';
        ftDateTime      : SubNode['type'] := 'datetime';
        ftTimeStamp     : SubNode['type'] := 'datetime';

        ftMemo          ,
        ftWideMemo      ,
        ftFmtMemo       ,
        ftOraClob       : SubNode['type'] := 'memo';

        ftGraphic       : SubNode['type'] := 'graphic';

        ftOraBlob       ,
        ftBlob          : SubNode['type'] := 'blob';
      end;

      // ftString, ftWideString, ftGuid, etc
      if (Field is TStringField) then
        SubNode['size']  := Field.FieldDef.Size.ToString();

      if Field.Required then
        SubNode['required'] := 'true';

      if Field.ReadOnly then
        SubNode['readonly'] := 'true';

      if ((Field is TFloatField) or (Field is TBCDField) or (Field is TFmtBCDField)) and (Field.FieldDef.Precision <> -1) then
        SubNode['precision'] := Field.FieldDef.Precision.ToString();
    end;

  end;


  // data ======================================================
  if not SchemaOnly then
  begin
    Node := AddNode(Root, 'data');
    Node['row_count'] := Table.RecordCount.ToString();

    Table.DisableControls;
    BM := Table.Bookmark;
    try
      Table.Last;
      Table.First;
      while not Table.Eof do
      begin
        SubNode := AddNode(Node, 'row');

        for i := 0 to Table.FieldCount - 1 do
        begin
          Field   := Table.Fields[i];
          if Field.FieldKind = fkData then
          begin
            TagName := 'c' + IntToStr(i);
            V := Table.FieldByName(Field.FieldName).Value;
            if VarIsNull(V) then
            ///if Self.FieldByName(Field.FieldName).IsNull then
              SubNode[TagName] := NULL_STR
            else begin
              case Field.DataType of
                ftString     ,
                ftWideString ,
                ftFixedWideChar,
                ftFixedChar  ,
                ftGuid       : SubNode[TagName] := StrToXml(Field.AsString);

                ftSmallint   ,
                ftInteger    ,
                ftWord       ,
                ftLargeint   ,
                ftAutoInc    : SubNode[TagName] := Field.AsString;

                ftBoolean    : SubNode[TagName] := AnsiLowerCase(Field.AsString);

                ftFloat      ,
                ftBCD        ,
                ftFMTBcd     ,
                ftCurrency   : SubNode[TagName] := FloatToStr(Field.AsFloat, FS);
                ftDate       : SubNode[TagName] := FormatDateTime('yyyy-mm-dd', Field.AsDateTime);
                ftTime       : SubNode[TagName] := FormatDateTime('hh:nn:ss', Field.AsDateTime);
                ftDateTime   ,
                ftTimeStamp  : SubNode[TagName] := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Field.AsDateTime);
                ftMemo       ,
                ftFmtMemo    ,
                ftWideMemo   ,
                ftGraphic    ,
                ftOraBlob    ,
                ftOraClob    ,
                ftBlob       : begin
                                 MS := TMemoryStream.Create;
                                 try
                                   TBlobField(Field).SaveToStream(MS);
                                   MS.Position := 0;
                                   SubNode[TagName] := StreamToBase64(MS);
                                 finally
                                   MS.Free;
                                 end;

                               end;
              end;
            end;
          end;

        end;

        Table.Next;
      end;
    finally
      Table.Bookmark := BM;
      Table.EnableControls;
    end;

  end;

end;

class function XmlPersistor.ToXmlText(Table: TDataset; SchemaOnly: Boolean): string;
var
  Doc: TXMLDocument;
  SS: TStringStream;
begin
  Doc := ToXmlDoc(Table, SchemaOnly);
  try
    SS := TStringStream.Create('');
    try
      WriteXMLFile(Doc, SS);
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  finally
    Doc.Free;
  end;
end;

class procedure XmlPersistor.SaveToXmlFile(Table: TDataset; const FileName: string; SchemaOnly: Boolean = False);
var
  Doc   : TXMLDocument;
begin
  Doc := ToXmlDoc(Table, SchemaOnly);
  try
    WriteXMLFile(Doc, FileName);
  finally
    Doc.Free();
  end;
end;

class procedure XmlPersistor.SaveToXmlStream(Table: TDataset; Stream: TStream; SchemaOnly: Boolean);
var
  Doc   : TXMLDocument;
begin
  Doc := ToXmlDoc(Table, SchemaOnly);
  try
    WriteXMLFile(Doc, Stream);
  finally
    Doc.Free();
  end;
end;

class procedure XmlPersistor.FromXmlDoc(Table: TDataset; Doc: TXMLDocument; SchemaOnly: Boolean = False);

  {-------------------------------------------------------------------}
  function AsVariant(const Value: Variant; Default: Variant): Variant;
  begin
    Result := Value;
    if VarIsNull(Result) then
      Result := Default;
  end;
  {-------------------------------------------------------------------}
  function GetFieldType(const sType: string): TFieldType;
  var
    FieldType : TFieldType;
  begin
     FieldType := ftString;

          if sType = 'string'     then FieldType := ftString
     else if sType = 'widestring' then FieldType := ftWideString
     else if sType = 'guid'       then FieldType := ftGuid
     else if sType = 'integer'    then FieldType := ftInteger
     else if sType = 'autoinc'    then FieldType := ftAutoInc
     else if sType = 'boolean'    then FieldType := ftBoolean
     else if sType = 'float'      then FieldType := ftFloat
     else if sType = 'money'      then FieldType := ftCurrency
     else if sType = 'date'       then FieldType := ftDate
     else if sType = 'time'       then FieldType := ftTime
     else if sType = 'datetime'   then FieldType := ftDateTime
     else if sType = 'memo'       then FieldType := ftMemo
     else if sType = 'graphic'    then FieldType := ftGraphic
     else if sType = 'blob'       then FieldType := ftBlob
     ;

     Result := FieldType;
  end;
  {-------------------------------------------------------------------}
  function GetAttr(const Node: TDOMNode; AttrName: string; DefaultValue: string): string;
  begin
    if (Node is TDOMElement) and (Assigned(TDOMElement(Node).GetAttributeNode(AttrName))) then
      Result := TDOMElement(Node)[AttrName]
    else
      Result := DefaultValue;
  end;
  {-------------------------------------------------------------------}
  function HasAttr(const Node: TDOMNode; AttrName: string): Boolean;
  begin
    Result := (Node is TDOMElement) and Assigned(TDOMElement(Node).GetAttributeNode(AttrName));
  end;

  {-------------------------------------------------------------------}
  function Base64ToStream(Input: string): TStream;
  var
    InputSS : TStringStream;
    Decoder : TBase64DecodingStream;
  begin
    Result  := TMemoryStream.Create;
    InputSS := TStringStream.Create(Input);
    try
      InputSS.Position := 0;
      Decoder := TBase64DecodingStream.Create(InputSS, bdmMIME);
      Result.CopyFrom(Decoder, Decoder.Size);
      Result.Position := 0;
    finally
      InputSS.Free;
    end;
  end;
  {-------------------------------------------------------------------}
var
  Root      : TDOMElement;
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  Nodes     : TDOMNodeList;
  i         : Integer;
  j         : Integer;
  FieldDef  : TFieldDef;
  Field     : TField;


  sValue    : string;

  MS        : TStream;
  FS        : TFormatSettings;

  FieldName : string;
  FieldType : TFieldType;
  FieldSize : Integer;
  Precision : Integer;
  Required  : Boolean;
  ReadOnly  : Boolean;
  FieldNo   : Integer;
  CodePage  : TSystemCodePage;
begin
  FS     := InvariantFormatSettings;

  Root   := Doc.DocumentElement;

  Table.Active := False;

  if HasProperty(Table, 'TableName') then
    TypInfo.SetStrProp(Table, 'TableName', GetAttr(Root, 'TableName', ''));

  // schema ======================================================
  Node  := Root.FindNode('schema') as TDOMElement;
  Nodes := Node.ChildNodes;

  for i := 0 to Nodes.Count - 1 do
  begin
    Precision := -1;
    FieldSize := 0;
    Required  := False;
    ReadOnly  := False;
    CodePage  := 0;
    FieldNo   := Table.FieldDefs.Count + 1;

    SubNode := Nodes[i] as TDOMElement;

    FieldName := SubNode['name'];
    FieldType := GetFieldType(SubNode['type']);
    if HasAttr(SubNode, 'size') then
      FieldSize        := LongInt(AsVariant(SubNode['size'], 0));

    if HasAttr(SubNode, 'required') then
      Required    := Boolean(AsVariant(SubNode['required'], False));

    if HasAttr(SubNode, 'readonly') then
      ReadOnly :=  Boolean(AsVariant(SubNode['readonly'], False));

    if HasAttr(SubNode, 'precision') then
      Precision :=  LongInt(AsVariant(SubNode['precision'], False));

    case FieldType of
      ftString, ftFixedChar, ftMemo:
        CodePage := CP_UTF8;
      ftWideString, ftFixedWideChar, ftWideMemo:
        CodePage := CP_UTF16;
    end;

    FieldDef := Table.FieldDefs.Add(FieldName, FieldType, FieldSize, Precision, Required, ReadOnly, FieldNo, CodePage);
  end;

  //CreateDatasetProc();
  Table.Active := True;

  // data ======================================================
  if not SchemaOnly then
  begin
    Table.Active := True;

    Node  := Root.FindNode('data') as TDOMElement;
    Nodes := Node.ChildNodes;

    for j := 0 to Nodes.Count - 1 do
    begin
      SubNode := Nodes[j] as TDOMElement;
      Table.Append();

      for i := 0 to Table.FieldCount - 1 do
      begin
        sValue := GetAttr(SubNode, 'c' + IntToStr(i), NULL_STR);

        Field  := Table.Fields[i];
        if sValue = NULL_STR then
          Field.Value := Variants.Null
        else begin
          case Field.DataType of
            ftString     ,
            ftWideString ,
            ftFixedWideChar,
            ftFixedChar  ,
            ftGuid       : Field.AsString := sValue;

            ftSmallint   ,
            ftInteger    ,
            ftWord       ,
            ftLargeint   ,
            ftAutoInc    : Field.AsInteger  := StrToInt(sValue);

            ftBoolean    : Field.Value      := StrToBool(sValue); // sValue; // = 'true';
            ftFloat      ,
            ftBCD        ,
            ftFMTBcd     ,
            ftCurrency   : Field.Value     := StrToFloat(sValue, FS);

            ftDate       : Field.Value     := StrToDate(sValue, FS);
            ftTime       : Field.Value     := StrToTime(sValue, FS);
            ftDateTime   ,
            ftTimeStamp  : Field.Value     := StrToDateTime(sValue, FS);
            ftMemo       ,
            ftWideMemo   ,
            ftFmtMemo    ,
            ftGraphic    ,
            ftOraBlob    ,
            ftOraClob    ,
            ftBlob       : begin
                             MS := Base64ToStream(sValue);
                             try
                               MS.Position := 0;
                               TBlobField(Field).LoadFromStream(MS);
                             finally
                               MS.Free;
                             end;

                           end;
          end;
        end;

      end;

      try
        Table.Post;
      except
        on E: Exception do
        begin
          E.Message := Format('Error while posting. Record number %d', [j]) + LineEnding + E.Message;
          raise;
        end;
      end;

    end;
  end;



end;

class procedure XmlPersistor.FromXmlText(Table: TDataset; XmlText: string; SchemaOnly: Boolean = False);
var
  Doc : TXMLDocument;
  SS  : TStringStream;
begin
  Doc := nil;

  SS := TStringStream.Create(XMLText);
  try
    ReadXMLFile(Doc, SS);
    try
      FromXmlDoc(Table, Doc, SchemaOnly);
    finally
      Doc.Free();
    end;
  finally
    SS.Free();
  end;
end;

class procedure XmlPersistor.LoadFromXmlFile(Table: TDataset; const FileName: string; SchemaOnly: Boolean = False);
var
  Doc   : TXMLDocument;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);

  ReadXMLFile(Doc, FileName);
  try
    FromXmlDoc(Table, Doc, SchemaOnly);
  finally
    Doc.Free();
  end;
end;

class procedure XmlPersistor.LoadFromXmlStream(Table: TDataset; Stream: TStream; SchemaOnly: Boolean);
var
  Doc   : TXMLDocument;
begin
  ReadXMLFile(Doc, Stream);
  try
    FromXmlDoc(Table, Doc, SchemaOnly);
  finally
    Doc.Free();
  end;
end;







{ TFieldInfo }
constructor TFieldInfo.Create(Field: TField; aIndex: Integer);
begin
  inherited Create();
  FField     := Field;
  FFieldName := Field.FieldName;
  FIsAutoInc := Field.DataType = ftAutoInc;
  FIsBlob    := TMemTable.IsBlobFieldType(Field.DataType);
  FIndex     := aIndex;
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
  BlobSize : PtrUInt;
  FieldInfo: TFieldInfo;
begin
  inherited Create();

  FDataset     := Dataset;
  FField       := Field;
  FRecBuf      := RecBuf;
  FMode        := Mode;
  FieldInfo    := Dataset.GetFieldInfo(Field);
  FFieldIndex  := FieldInfo.Index;

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
    FDataset.DataEvent(deFieldChange, PtrUInt(FField));
  except
    FDataset.InternalHandleException();
  end;

  SetSize(0);
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

  FAllRows           := TList.Create();
  FRows              := TList.Create();

  FFields            := TList.Create();
  FSortOnFields      := TList.Create();
  FDetailFields      := TList.Create();
  FRangeFields       := TList.Create();
  FModifiedFields    := TList.Create();

  FMasterLink                 := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange  := Addr(OnMasterLinkChanged);
  FMasterLink.OnMasterDisable := Addr(OnMasterLinkDisabled);

  FStatusOptions    := [usModified, usInserted, usUnmodified];

  FFilterParser      := TFilterParser.Create();
  FFilterParser.OnVariable := @OnFilterVariableValueEvent;

  FCurRecIndex      := -1;

  FAutoIncEnabled   := True;
end;

destructor TMemTable.Destroy;
begin
  FreeAndNil(FMasterLink);

  inherited Destroy;

  FreeAndNil(FFilterParser);

  FreeAndNil(FModifiedFields);
  FreeAndNil(FRangeFields);
  FreeAndNil(FDetailFields);
  FreeAndNil(FSortOnFields);
  FreeAndNil(FFields);

  FreeAndNil(FRows);
  FreeAndNil(FAllRows);
end;



function TMemTable.AllocRecordBuffer(): TRecordBuffer;
var
  RecInfo: PRecInfo;
begin
  Result := AllocMem(FRecBufSize);
  InitRecord(Result);

  RecInfo := GetRecInfo(Result);
  RecInfo^.Id := FLastRecId;
  Inc(FLastRecId);
end;

procedure TMemTable.FreeRecordBuffer(var RecBuf: TRecordBuffer);
begin
  if FBlobCount > 0 then
    FreeBlobs(RecBuf);

  FreeMem(RecBuf, FRecBufSize);
  RecBuf := nil;
end;

function TMemTable.GetRecord(RecBuf: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  SourceRecBuf  : TRecordBuffer;
  RecCount      : Integer;
  RecInfo       : PRecInfo;
begin

  RecCount := FRows.Count;

  SourceRecBuf := nil;

  if RecCount < 1 then
  begin
     Result := grEOF
  end else begin

     Result := grOk;

     case GetMode of
       gmPrior   : begin
                     if FCurRecIndex <= 0 then
                       Result := grBOF
                     else
                       FCurRecIndex := FCurRecIndex - 1;
                   end;

       gmCurrent : begin
                     if (FCurRecIndex < 0) or (FCurRecIndex >= RecCount) then
                       Result := grError;
                   end;

       gmNext    : begin
                     if FCurRecIndex >= RecCount - 1 then
                       Result := grEOF
                     else
                       FCurRecIndex := FCurRecIndex + 1;
                   end;

     end;

     if (Result = grOK) and Assigned(RecBuf) then
     begin
       SourceRecBuf := FRows[FCurRecIndex];
       CopyRecord(SourceRecBuf, RecBuf, True);

       RecInfo := GetRecInfo(RecBuf);
       RecInfo^.BookmarkFlag := bfCurrent;
     end else if (Result = grError) and DoCheck then
       DatabaseError('No records');
   end;

end;

function TMemTable.GetRecordSize(): Word;
begin
   Result := FRecBufSize;
end;

function TMemTable.GetBookmarkFlag(RecBuf: TRecordBuffer): TBookmarkFlag;
var
  RecInfo: PRecInfo;
begin
  RecInfo := GetRecInfo(RecBuf);
  Result  := RecInfo^.BookmarkFlag;
end;

procedure TMemTable.SetBookmarkFlag(RecBuf: TRecordBuffer; Value: TBookmarkFlag);
var
  RecInfo: PRecInfo;
begin
  RecInfo := GetRecInfo(RecBuf);
  RecInfo^.BookmarkFlag := Value;
end;

procedure TMemTable.GetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  RecInfo : PRecInfo;
  BM      : TIntBM;
begin
  RecInfo := GetRecInfo(RecBuf);
  BM := RecInfo^.Bookmark;
  PIntBM(Data)^ := BM;
end;

procedure TMemTable.SetBookmarkData(RecBuf: TRecordBuffer; Data: Pointer);
var
  RecInfo : PRecInfo;
  BM      : TIntBM;
begin
  BM := PIntBM(Data)^;

  RecInfo := GetRecInfo(RecBuf);
  RecInfo^.Bookmark := BM;
end;

procedure TMemTable.InternalGotoBookmark(pBM: Pointer);
var
  BM : TIntBM;
begin
  BM := PIntBM(pBM)^;
  GoToBookmarkInternal(BM);
end;

procedure TMemTable.InternalSetToRecord(RecBuf: TRecordBuffer);
var
  RecInfo : PRecInfo;
  BM      : TIntBM;
begin
  RecInfo := GetRecInfo(RecBuf);

  BM := RecInfo^.Bookmark;
  GoToBookmarkInternal(BM);
end;

function TMemTable.GoToBookmarkInternal(BM: TIntBM): Boolean;
var
  Index  : Integer;
begin
  Result := False;

  Index := IndexOfBookmark(BM);

  if Index <> -1 then
  begin
    Result := True;
    FCurRecIndex := Index;
  end;
end;

function TMemTable.GetBookmarkInternal(RecBuf: Pointer): TIntBM;
var
  RecInfo: PRecInfo;
begin
  RecInfo := GetRecInfo(RecBuf);
  Result  := RecInfo^.Bookmark;
end;

function TMemTable.IndexOfBookmark(BM: TIntBM): Integer;
var
  i     : Integer;
  RecInfo: PRecInfo;
begin
  Result := -1;
  if BM > 0 then
  begin
    for i := 0 to FRows.Count - 1 do
    begin
      RecInfo := GetRecInfo(FRows[i]);
      if (BM = RecInfo^.Bookmark) then
      begin
        Result := i;
        Break; //==>
      end;
    end;
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
    CursorPosChanged();
    RecBuf := AllocRecordBuffer();
    try
      Result := Assigned(BM) and (IndexOfBookmark(PIntBM(BM)^) <> -1) and (GetRecord(RecBuf, gmCurrent, False) = grOK);
    finally
      FreeRecordBuffer(RecBuf);
    end;
  end;

end;

function TMemTable.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  Index1 : Integer;
  Index2 : Integer;
begin
  CheckActive();

  if (Bookmark1 = nil) and (Bookmark2 = nil) then
    Exit(0);
  if (Bookmark1 <> nil) and (Bookmark2 = nil) then
    Exit(1);
  if (Bookmark1 = nil) and (Bookmark2 <> nil) then
    Exit(-1);

  Index1 := IndexOfBookmark(PIntBM(Bookmark1)^);
  Index2 := IndexOfBookmark(PIntBM(Bookmark2)^);

  if Index1 < Index2 then
     Exit(-1);

  if Index1 > Index2 then
    Exit(1);

  Exit(0);
end;

procedure TMemTable.InternalFirst;
begin
  FCurRecIndex := -1;
end;

procedure TMemTable.InternalLast;
begin
  FCurRecIndex := FRows.Count;
end;

procedure TMemTable.InternalInitRecord(RecBuf: TRecordBuffer);
begin
   FillChar(RecBuf^, FRecBufSize, 0);  // no, we do NOT need this, AllocMem() fills buffer with zeroes
end;

procedure TMemTable.InternalPost;
var
  RecBuf          : TRecordBuffer;
  DestRecBuf      : TRecordBuffer;

  SourceRecInfo   : PRecInfo;
  DestRecInfo     : PRecInfo;
begin
  inherited InternalPost(); // checks required fields

  RecBuf := TRecordBuffer(ActiveBuffer());
  SourceRecInfo := GetRecInfo(RecBuf);

  if State = dsEdit then
  begin
    DestRecBuf := FRows[FCurRecIndex];
    DestRecInfo := GetRecInfo(DestRecBuf);

    CopyRecord(RecBuf, DestRecBuf, False);

    DestRecInfo^.Bookmark      := SourceRecInfo^.Bookmark;
    DestRecInfo^.BookmarkFlag  := SourceRecInfo^.BookmarkFlag;
    if DestRecInfo^.Status = usUnmodified then
      DestRecInfo^.Status := usModified;

    if not CanDisplayRecord(RecBuf) then
    begin
      FRows.Remove(RecBuf);
      FCurRecIndex := Min(FRows.Count - 1, FCurRecIndex)
    end else begin
      Sort();
    end;
  end else begin
    InternalAddRecord(RecBuf, False);
  end;

  FModifiedFields.Clear;

end;

procedure TMemTable.InternalAddRecord(RecBuf: Pointer; IsAppend: Boolean);

  function GetNextAutoIncValue(FieldIndex: Integer): Integer;
  var
    i             : Integer;
    RecBuf2       : TRecordBuffer;
    P             : PByte;
    V             : Integer;
  begin
    Result := 0;

    for i := 0 to FAllRows.Count -1 do
    begin
      RecBuf2 := TRecordBuffer(FAllRows[i]);
      if not IsFieldNull(RecBuf2, FieldIndex) then
      begin
        P := GetFieldPtr(RecBuf2, FieldIndex);        // field buffer
        Inc(P);                                       // skip the null flag
        V := PInteger(P)^;
        Result := Max(Result, V);
      end;
    end;

    Result := Result + 1;
  end;

  procedure AssignAutoIncValues();
  var
    i             : Integer;
    FieldInfo     : TFieldInfo;
    P             : PByte;
    V             : Integer;
    FieldIndex    : Integer;
  begin
    for i := 0 to FFields.Count - 1 do
    begin
      FieldInfo := TFieldInfo(FFields[i]);
      if FieldInfo.IsAutoInc then
      begin
        FieldIndex := FieldInfo.Index;
        V := GetNextAutoIncValue(FieldIndex);
        P := GetFieldPtr(RecBuf, FieldIndex);         // field buffer
        Inc(P);                                       // skip the null flag
        PInteger(P)^ := V;
        SetFieldNullFlag(RecBuf, FieldIndex, False);
      end;
    end;
  end;

var
  DestRecBuf    : PByte;
  RecIndex      : Integer;

  SourceRecInfo       : PRecInfo;
  //DestRecInfo         : PRecInfo;
begin
  if FCurRecIndex < 0 then
     FCurRecIndex := 0;

  if AutoIncEnabled then
     AssignAutoIncValues();

  Inc(FLastBookmark);

  SourceRecInfo := GetRecInfo(RecBuf);
  SourceRecInfo^.Bookmark      := FLastBookmark;     // CRUCIAL: this must be set
  SourceRecInfo^.BookmarkFlag  := bfCurrent;
  SourceRecInfo^.Status        := usInserted;

  DestRecBuf     := Pointer(AllocRecordBuffer());

  CopyRecord(RecBuf, DestRecBuf, True);

  FAllRows.Add(DestRecBuf);

  if CanDisplayRecord(DestRecBuf) then
  begin
    RecIndex := FCurRecIndex;
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
  FreeBlobs(TRecordBuffer(ActiveBuffer()));
end;

procedure TMemTable.InternalDelete;
var
  RecBuf    : TRecordBuffer;
  RowStatus : TUpdateStatus;
  RecInfo   : PRecInfo;
begin
  RecBuf := FRows[FCurRecIndex];

  RecInfo := GetRecInfo(RecBuf);
  RowStatus := RecInfo^.Status;   // usUnmodified, usModified, usInserted, usDeleted

  if RowStatus in [usInserted] then
  begin
    FAllRows.Remove(RecBuf);
    FreeRecordBuffer(RecBuf);
  end else begin
    RecInfo^.Status := usDeleted;  // keep deleted rows in AllRows
  end;

  FRows.Delete(FCurRecIndex);
  FCurRecIndex := Min(FRows.Count - 1, FCurRecIndex);
end;

function TMemTable.IsCursorOpen: Boolean;
begin
  Result := FInitialized;
end;

procedure TMemTable.InternalOpen;
var
  i : Integer;
begin
  // InternalInitFieldDefs();                  // does nothing in our implementation

  { ignore DefaultFields flag.
    if DefaultFields is True then we have to create Fields based on FieldDefs.
    This dataset handles this situation differently. }
  // if DefaultFields then
  //  CreateFields();

  if (Fields.Count = 0) and (FieldDefs.Count = 0) then
    DatabaseError(SSchemaNotDefined);

  if (Fields.Count = 0) and (FieldDefs.Count > 0) then
    CreateFields();                            // create Fields based on FieldDefs

  if (Fields.Count > 0) then
  begin
    FieldDefs.Clear();
    InitFieldDefsFromFields();                 // create FieldDefs based on Fields
  end;

  for i := 0 to FieldCount - 1 do
    if not IsSupportedFieldType(Fields[i].DataType) then
      DatabaseErrorFmt(SUnsupportedFieldType, [Fields[i].DisplayName], Self);

  BindFields(True);

  Self.Initialize();
end;

procedure TMemTable.InternalClose;
begin
  Self.Finalize();

  if DefaultFields then
    DestroyFields;
end;

procedure TMemTable.InternalInitFieldDefs;
begin
   { nothing - TFieldDef instances must be provided by client code }
end;

procedure TMemTable.ClearCalcFields(RecBuf: TRecordBuffer);
var
  i         : Integer;
  FieldInfo : TFieldInfo;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FFields[i]);
    if not (FieldInfo.Field.FieldKind in [fkData, fkInternalCalc]) then
    begin
      SetFieldDataInternal(RecBuf, nil, FieldInfo.Index);
    end;
  end;

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
    if (Fields[i].DataType <> ftLargeInt) and (Fields[i].DefaultExpression <> '') then { VCL does not fully supports LargeInt Variants }
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

function TMemTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf      : Pointer;
  FieldInfo   : TFieldInfo;
  FieldIndex  : Integer;
begin
  Result := False;
  RecBuf := nil;
  if GetActiveRecBuf(RecBuf) then
  begin
    FieldInfo  := GetFieldInfo(Field);
    FieldIndex := FieldInfo.Index;

    if Field.FieldKind in [fkData, fkInternalCalc] then
    begin
      if (not ((State = dsBrowse) and IsEmpty)) then
        Result := GetFieldDataInternal(RecBuf, Buffer, FieldIndex);
    end else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Result := GetFieldDataInternal(RecBuf, Buffer, FieldIndex)
    end;
  end;

end;

procedure TMemTable.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf      : Pointer;
  FieldInfo   : TFieldInfo;
  FieldIndex  : Integer;
begin
  RecBuf := nil;

  if not (State in dsWriteModes) then
    DatabaseErrorFmt(SNotEditing, [Self.Name], Self);

  GetActiveRecBuf(RecBuf);

  FieldInfo  := GetFieldInfo(Field);
  FieldIndex := FieldInfo.Index;

  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SReadOnlyField, [Field.DisplayName], Self);

    Field.Validate(Buffer);

    if FModifiedFields.IndexOf(Field) = -1 then
      FModifiedFields.Add(Field);

    SetFieldDataInternal(RecBuf, Buffer, FieldIndex);

  end else if (State <> dsInternalCalc) then
  begin
    SetFieldDataInternal(RecBuf, Buffer, FieldIndex);
  end;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, PtrUInt(Field));
end;

function TMemTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  RecBuf: Pointer;
begin
  RecBuf := nil;
  GetActiveRecBuf(RecBuf);
  Result := TBlobStream.Create(Self, TBlobField(Field), RecBuf, Mode);
end;

procedure TMemTable.Initialize;

  function GetFieldBufferSize(FieldInfo: TFieldInfo): Integer;
  var
    Field: TField;
  begin
    Result := 0;
    Field  := FieldInfo.Field;

    if not IsBlobFieldType(Field.DataType) then
      Result := Field.DataSize
    else
      Result := SizeOf(TBlob);
  end;
  {
  function GetFieldBufferSize2(FieldInfo: TFieldInfo): Integer;
  var
    Field: TField;
  begin
    Result := 0;
    Field  := FieldInfo.Field;

    case Field.DataType of
      ftString        : Result := Field.DataSize + 2; // (Field.Size + 1) * SizeOf(AnsiChar); // SizeOf(AnsiChar);  Field.FieldDef.CharSize
      ftWideString    : Result := (Field.Size + 1) * SizeOf(WideChar); // SizeOf(WideChar);
      ftGuid          : Result := (Field.Size + 1) * SizeOf(AnsiChar); // SizeOf(AnsiChar);

      ftFixedChar     : Result := (Field.Size + 1) * SizeOf(AnsiChar); // SizeOf(AnsiChar);
      ftFixedWideChar : Result := (Field.Size + 1) * SizeOf(WideChar); // SizeOf(WideChar);

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
  }

var
  i               : Integer;
  FieldInfo       : TFieldInfo;
  Field           : TField;
  BufferIndex     : TKeyBufferIndex;

  DataFields      : TList;
  CalcFields      : TList;
  BlobFields      : TList;

  FieldBufferSize : LongWord;

  Ofs             : LongWord;

  DataSize        : LongWord;                 { plain data fields total size, i.e. fkData, fkInternalCalc - NO Calculated or Blob fields }
  CalcsSize       : LongWord;                 { calc fields total size, i.e. fkCalculated, fkLookup }
  BlobsSize       : LongWord;                 { blob fields total size }

  IsBlob          : Boolean;
  NullSize        : LongWord;
begin
  AddFieldInfos();


  LoadSortOnFieldList;

  try
    if FInitialized then
      Finalize();

    BookmarkSize   := SizeOf(TIntBM);
    FCurRecIndex   := -1;
    FLastRecId      := 1;

    DataFields     := TList.Create();
    CalcFields     := TList.Create();
    BlobFields     := TList.Create();

    { separate the fields }
    for i := 0 to FFields.Count - 1 do
    begin
      FieldInfo := TFieldInfo(FFields[i]);
      Field     := FieldInfo.Field;
      IsBlob    := IsBlobFieldType(Field.DataType);

      if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
        DataFields.Add(FieldInfo)
      else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up and calc fields }
        CalcFields.Add(FieldInfo)
      else if IsBlob then                                                                { blob fields }
        BlobFields.Add(FieldInfo)
      ;
    end;

    FBlobCount := BlobFields.Count;

    { recreate the FieldInfo list according to the record buffer layout }
    FFields.Clear;                                                                       { just clear the list, do NOT free the TFieldInfo instances }
    AddList(DataFields, FFields);
    AddList(CalcFields, FFields);
    AddList(BlobFields, FFields);

    SetLength(FFieldTypes        , FFields.Count);
    SetLength(FFieldBufferSizes  , FFields.Count);
    SetLength(FOffsets           , FFields.Count);

    { process the FieldInfo list, feed the property arrays, calculate FieldInfo offsets, and FieldInfo section sizes }
    FRecBufSize := 0;
    Ofs         := 0;
    DataSize    := 0;
    CalcsSize   := 0;
    BlobsSize   := 0;

    { now FieldInfo fields are in distinct sections, according to buffer layout, i.e. data, calc and blob fields }
    for i := 0 to FFields.Count - 1 do
    begin
      FieldInfo        := TFieldInfo(FFields[i]);
      FieldInfo.FIndex := i;                        // correct the index
      Field            := FieldInfo.Field;
      IsBlob           := IsBlobFieldType(Field.DataType);
      FieldBufferSize  := GetFieldBufferSize(FieldInfo);

      { increase size by 1 for the null flag. Blobs have Size property in TBlob structure, no null flag is needed }
      NullSize := 0;
      if not IsBlob then
        NullSize := 1;

      FFieldTypes[i]         := Field.DataType;
      FFieldBufferSizes[i]   := FieldBufferSize;

      FOffsets[i]            := Ofs;
      Ofs                    := Ofs + FieldBufferSize + NullSize;

      if (Field.FieldKind in [fkData, fkInternalCalc]) and (not IsBlob) then             { plain data fields }
        DataSize  := DataSize  + FieldBufferSize + NullSize
      else if (not (Field.FieldKind in [fkData, fkInternalCalc])) and (not IsBlob) then  { look up fields }
        CalcsSize := CalcsSize + FieldBufferSize + NullSize
      else if IsBlob then                                                                { blob fields }
        BlobsSize := BlobsSize + FieldBufferSize + NullSize;
    end;

    { section offsets and total record buffer size }
    FCalcOfs      := DataSize;
    FBlobOfs      := FCalcOfs + CalcsSize ;
    FBookOfs      := FBlobOfs + BlobsSize;
    FRecBufSize   := FBookOfs + SizeOf(TRecInfo);

    { initialize key buffers }
    for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
      FKeyBuffers[BufferIndex] := Pointer(AllocRecordBuffer());

    FInitialized               := True;
  finally
    DataFields.Free;
    CalcFields.Free;
    BlobFields.Free;
  end;
end;

procedure TMemTable.Finalize;
var
  BufferIndex : TKeyBufferIndex;
begin
  if (FInitialized) then
  begin
    { free key buffers }
    for BufferIndex := Low(TKeyBufferIndex) to High(TKeyBufferIndex) do
      FreeMem(FKeyBuffers[BufferIndex], FRecBufSize);

    DeleteRows();

    ClearObjectList(FFields);

    FRecBufSize                := 0;
    FBookOfs                   := 0;
    FBlobCount                 := 0;

    FFieldTypes                := nil;
    FFieldBufferSizes          := nil;
    FOffsets                   := nil;

    FInitialized := False;
  end;

end;

procedure TMemTable.DeleteRows;
var
  i      : Integer;
  RecBuf : TRecordBuffer;
begin
  CancelUpdates();

  for i := 0 to FAllRows.Count - 1 do
  begin
    RecBuf  := FAllRows[i];
    FreeRecordBuffer(RecBuf);
  end;

  FRows.Clear();
end;
function  TMemTable.GetRecInfo(RecBuf: Pointer): PRecInfo;
var
  P : PByte;
begin
  P := PByte(RecBuf) + FBookOfs;
  Result := PRecInfo(P);
end;

function  TMemTable.GetFieldPtr(RecBuf: Pointer; FieldIndex: Integer): PByte;
begin
  Result := PByte(RecBuf) + FOffsets[FieldIndex];
end;

function  TMemTable.GetBlobPtr(RecBuf: Pointer; FieldIndex: Integer): PBlob;
var
  P : PByte;
begin
  P := GetFieldPtr(RecBuf, FieldIndex);
  Result := PBlob(P);
end;

function TMemTable.IsFieldNull(RecBuf: Pointer; FieldIndex: Integer): Boolean;
var
  P : PByte;
  Blob : PBlob;
begin
  if not IsBlobFieldType(FFieldTypes[FieldIndex]) then
  begin
    P := GetFieldPtr(RecBuf, FieldIndex);
    Result := PByte(P)^ = 0;
  end else begin
    Blob   := GetBlobPtr(RecBuf, FieldIndex);
    Result := Blob^.Size = 0;
  end;
end;

procedure TMemTable.SetFieldNullFlag(RecBuf: Pointer; FieldIndex: Integer; ToNull: Boolean);
var
  P : PByte;
begin
  if not IsBlobFieldType(FFieldTypes[FieldIndex]) then
  begin
    P := GetFieldPtr(RecBuf, FieldIndex);

    if ToNull then
      PByte(P)^ := 0
    else
      PByte(P)^ := 1;
  end;
end;

function TMemTable.GetFieldDataInternal(RecBuf: Pointer; Buffer: Pointer; const FieldIndex: Integer): Boolean;
var
  P          : PByte;
  HasData    : Boolean;
begin
  HasData := not IsFieldNull(RecBuf, FieldIndex);

  if Assigned(Buffer) then
  begin
    HasData    := not IsFieldNull(RecBuf, FieldIndex);

    if HasData then
    begin
      FillChar(Buffer^, FFieldBufferSizes[FieldIndex], 0);

      P := GetFieldPtr(RecBuf, FieldIndex);
      Inc(P);                                                  // skip the null flag byte
      CopyMem(P, Buffer, FFieldBufferSizes[FieldIndex]);
    end;
  end;

  Result := HasData;
end;

procedure TMemTable.SetFieldDataInternal(RecBuf: Pointer; Buffer: Pointer; const FieldIndex: Integer);
var
  P             : PByte;
  HasData       : Boolean;
  S             : string;
begin
  P         := GetFieldPtr(RecBuf, FieldIndex);
  HasData   := Assigned(Buffer);

  if FFieldTypes[FieldIndex] = ftString then
  begin
    SetString(S, Buffer, FFieldBufferSizes[FieldIndex]);
  end;

  if HasData then
  begin
    Inc(P);                                                     // skip the null flag byte
    CopyMem(Buffer, P, FFieldBufferSizes[FieldIndex]);
  end else begin
    FillChar(P^, FFieldBufferSizes[FieldIndex], 0);
  end;

  SetFieldNullFlag(RecBuf, FieldIndex, not HasData);
end;

procedure TMemTable.CopyFieldData(SourceRecBuf, DestRecBuf: Pointer; const FieldIndex: Integer);
var
  FieldInfo: TFieldInfo;
begin
  FieldInfo  := GetFieldInfo(FieldIndex);
  CopyFieldData(SourceRecBuf, DestRecBuf, FieldInfo);
end;

procedure TMemTable.CopyFieldData(SourceRecBuf, DestRecBuf: Pointer; FieldInfo: TFieldInfo);
var
  HasData    : Boolean;
  P          : PByte;
  BlobSize   : PtrUInt;
  FieldIndex : Integer;
  SourceBlob : PBlob;
begin
  FieldIndex := FieldInfo.Index;
  HasData    := not IsFieldNull(SourceRecBuf, FieldIndex);

  P          := nil;
  BlobSize   := 0;

  if not FieldInfo.IsBlob then
  begin
    if HasData then
    begin
      P  := SourceRecBuf + FOffsets[FieldIndex];
      Inc(P);                                               // skip the null flag byte
    end;

    SetFieldDataInternal(DestRecBuf, P, FieldIndex);
  end else begin
    if HasData then
    begin
      SourceBlob := GetBlobPtr(SourceRecBuf, FieldIndex);
      BlobSize   := SourceBlob^.Size;
      P          := SourceBlob^.Data;
    end;

    SetBlobData(DestRecBuf, P, FieldIndex, BlobSize);
  end;

end;

function TMemTable.GetBlobData(RecBuf, Buffer: Pointer; FieldIndex: Integer): PtrUInt;
var
  Blob: PBlob;
  Data: Pointer;
begin
  Blob   := GetBlobPtr(RecBuf, FieldIndex);

  Result := Blob^.Size;
  Data   := Blob^.Data;
  if  (Result > 0) and Assigned(Buffer) then
  begin
    CopyMem(Data, Buffer, Result);
  end;
end;

procedure TMemTable.SetBlobData(RecBuf, Buffer: Pointer; FieldIndex: Integer; BlobSize: PtrUInt);
var
  Blob: PBlob;
  HasData: Boolean;
begin
  Blob   := GetBlobPtr(RecBuf, FieldIndex);

  // first de-allocate any existing blob data
  if Blob^.Size > 0 then
    FreeBlob(Blob);

  // write the new data, if any
  HasData := (BlobSize > 0) and Assigned(Buffer);
  if HasData then
  begin
    //ReAllocMem(Blob^.Data, BlobSize);
    Blob^.Data := AllocMem(BlobSize);
    Blob^.Size := BlobSize;

    CopyMem(Buffer, Blob^.Data, BlobSize);
  end;

end;

function TMemTable.GetBlobSize(RecBuf: Pointer; FieldIndex: Integer): PtrUInt;
var
  Blob: PBlob;
begin
  Blob   := GetBlobPtr(RecBuf, FieldIndex);
  Result := Blob^.Size;
end;

procedure TMemTable.FreeBlobs(RecBuf: Pointer);
var
  i         : Integer;
  Blob      : PBlob;
  FieldInfo : TFieldInfo;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FFields[i]);
    if FieldInfo.IsBlob then
    begin
      Blob   := GetBlobPtr(RecBuf, FieldInfo.Index);
      FreeBlob(Blob);
    end;
  end;
end;

procedure TMemTable.FreeBlob(Blob: PBlob);
begin
  if Assigned(Blob) then
  begin
    if Blob^.Size > 0 then
      FreeMem(Blob^.Data, Blob^.Size);

    Blob^.Size := 0;
    Blob^.Data := nil;
  end;
end;

procedure TMemTable.CopyRecord(SourceRecBuf, DestRecBuf: Pointer; CopyRecInfoToo: Boolean);
var
  i             : Integer;
  FieldInfo     : TFieldInfo;
  SourceRecInfo : PRecInfo;
  DestRecInfo   : PRecInfo;
begin

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FFields[i]);
    CopyFieldData(SourceRecBuf, DestRecBuf, FieldInfo);
  end;

  if CopyRecInfoToo then
  begin
    SourceRecInfo  := GetRecInfo(SourceRecBuf);
    DestRecInfo    := GetRecInfo(DestRecBuf);

    DestRecInfo^.Bookmark      := SourceRecInfo^.Bookmark;
    DestRecInfo^.BookmarkFlag  := SourceRecInfo^.BookmarkFlag;
    DestRecInfo^.Status        := SourceRecInfo^.Status;
  end;
end;

function TMemTable.GetActiveRecBuf(var RecBuf: Pointer): Boolean;
begin
  case State of
    dsBlockRead    ,
    dsBrowse       : if IsEmpty or (BookmarkSize = 0) then
                       RecBuf := nil
                     else
                       RecBuf := Pointer(ActiveBuffer());
    dsEdit         ,
    dsInsert       : RecBuf := Pointer(ActiveBuffer());

    dsCalcFields   ,
    dsInternalCalc : RecBuf := Pointer(CalcBuffer);
    dsFilter       : RecBuf := FFilterBuffer;

    dsNewValue     ,
    dsOldValue     ,
    dsCurValue     : DatabaseError('GetActiveRecBuf');
    else             RecBuf := nil;

  end;

  Result := RecBuf <> nil;
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

procedure TMemTable.OnMasterLinkChanged(Sender: TObject);
begin

  CheckBrowseMode;

  if Assigned(FMasterLink.DataSet)
    and FMasterLink.DataSet.Active
    and (not (FMasterLink.DataSet.State in [dsEdit, dsInsert]))
    and (FMasterLink.Fields.Count > 0) then
  begin
    FDetailFields.Clear();
    GetFieldInfoList(FDetailFields, FDetailFieldNames);

    if (FMasterLink.Fields.Count = FDetailFields.Count) then
    begin
      SetLink(True);
      First();
    end;
  end;

end;

procedure TMemTable.OnMasterLinkDisabled(Sender: TObject);
begin
  SetLink(False);
  if Active then
    First();
end;
procedure TMemTable.SetLink(Value: Boolean);
var
  i                : Integer;
  MasterField      : TField;
  FieldInfo        : TFieldInfo;
  FieldIndex       : Integer;
  FieldBuf         : Pointer;
begin

  if Value then          // =========== Link
  begin
    { clear the master record buffer }
    FillChar(FKeyBuffers[biMaster]^, FRecBufSize, 0);

    for i := 0 to FDetailFields.Count - 1 do
    begin
      MasterField  := TField(FMasterLink.Fields[i]);

      FieldInfo    := TFieldInfo(FDetailFields[i]);
      FieldIndex   := FieldInfo.Index;
      FieldBuf     := AllocMem(FFieldBufferSizes[FieldIndex]);
      try
        MasterField.GetData(FieldBuf);                                            // get the master field buffer
        SetFieldDataInternal(FKeyBuffers[biMaster], FieldBuf, FieldIndex);        // put the master field buffer in FKeyBuffers
      finally
        FreeMem(FieldBuf);
      end;
    end;

    FModes := FModes + [cmLink];
  end else begin         // =========== Un-Link
    FModes := FModes - [cmLink];
  end;

  Rebuild();
end;

procedure TMemTable.Rebuild;
var
  i      : Integer;
  CurBuf : Pointer;
  RecBuf : Pointer;
  Index  : Integer;
begin

  if FInitialized then
  begin
    if (FCurRecIndex < 0) or (FCurRecIndex >= FRows.Count) then
      CurBuf := nil
    else
      CurBuf := FRows[FCurRecIndex] ;

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
      FCurRecIndex := Index;
      Resync([rmExact, rmCenter]);
      DoAfterScroll;
    end;

  end;

end;

function TMemTable.CanDisplayRecord(RecBuf: Pointer): Boolean;
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


  { filter }
  if (cmFilter in FModes) then
  begin
    SavedState    := SetTempState(dsFilter);

    FFilterBuffer := AllocRecordBuffer();
    CopyRecord(RecBuf, FFilterBuffer, True);

    try
      if Assigned(OnFilterRecord) then
      begin
        Accept := True;
        OnFilterRecord(Self, Accept);
        if not Accept then
          Exit(False);  //==>
      end;

      if Assigned(FFilterParser) and (Length(Filter) > 0) then
      begin
        if not FilterCanDisplayRecord(FFilterBuffer) then
           Exit(False);  //==>
      end;
    finally
      RestoreState(SavedState);
      FreeRecordBuffer(FFilterBuffer);
      FFilterBuffer := nil;
    end;
  end;


  Result := True;
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

procedure TMemTable.CancelUpdates();
var
  TempList : TList;
  i        : Integer;
  RecBuf   : TRecordBuffer;
  RecInfo  : PRecInfo;
begin
  if Active then
  begin
    TempList := TList.Create;
    try
      AddList(FAllRows, TempList);
      FAllRows.Clear;

      for i := 0 to TempList.Count - 1 do
      begin
        RecBuf  := TempList[i];
        RecInfo := GetRecInfo(RecBuf);

        if RecInfo^.Status <> usDeleted then           // usInserted and usModified
        begin
          RecInfo^.Status := usUnmodified;
          FAllRows.Add(RecBuf);
        end else begin
          FreeRecordBuffer(RecBuf);                    // usDeleted
        end;
      end;

    finally
      TempList.Free;
    end;

    First;

  end;
end;

procedure TMemTable.AddFieldInfos;
var
  i : Integer;
  FieldInfo : TFieldInfo;
begin
  Active := False;

  for i := 0 to Fields.Count - 1 do
  begin
    FieldInfo := TFieldInfo.Create(Fields[i], i);
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

function  TMemTable.GetFieldInfo(Field: TField): TFieldInfo;
var
  i : Integer;
  FieldInfo : TFieldInfo;
begin
  Result := nil;
  for i := 0 to FFields.Count - 1 do
  begin
     FieldInfo := TFieldInfo(FFields[i]);
    if Field = FieldInfo.Field then
    begin
      Result := FieldInfo;
      Exit; //==>
    end;
  end;

  raise Exception.CreateFmt('Field not found: %s', [Field.FieldName]);
end;

function  TMemTable.GetFieldInfo(const FieldIndex: Integer): TFieldInfo;
var
  i : Integer;
  FieldInfo: TFieldInfo;
begin

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FFields[i]);
    if FieldInfo.Index = FieldIndex then
    begin
      Result := FieldInfo;
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Field not found by Index: %d', [FieldIndex]);
end;

function TMemTable.GetFieldInfo(const FieldName: string): TFieldInfo;
var
  i : Integer;
  FieldInfo: TFieldInfo;
begin
  Result := nil;

  for i := 0 to FFields.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FFields[i]);
    if AnsiSameText(FieldName, FieldInfo.Field.FieldName) then
    begin
      Result := FieldInfo;
      Exit; //==>
    end;
  end;

  raise Exception.CreateFmt('Field not found: %s', [FieldName]);
end;
{ FieldNames must be a ; delimited list }
procedure TMemTable.GetFieldInfoList(List: TList; const FieldNames: string);
var
  Pos       : Integer;
  FieldInfo : TFieldInfo;
begin
  if Assigned(List) and (FieldNames <> '') then
  begin
    Pos := 1;
    while Pos <= Length(FieldNames) do
    begin
      FieldInfo := GetFieldInfo(ExtractFieldName(FieldNames, Pos));
      List.Add(FieldInfo);
    end;
  end;

end;

function TMemTable.GetFieldInfoList(const FieldNames: string): TList;
begin
  Result := TList.Create();
  GetFieldInfoList(Result, FieldNames);
end;



function TMemTable.UpdateStatus: TUpdateStatus;
var
  RecBuf : Pointer;
  RecInfo: PRecInfo;
begin
  CheckActive;

  if State = dsInternalCalc then
    Result := usUnModified
  else
  begin
    if State = dsCalcFields then
      RecBuf := Pointer(CalcBuffer)
    else
      RecBuf := Pointer(ActiveBuffer());

    RecInfo := GetRecInfo(RecBuf);
    Result  := RecInfo^.Status;
  end;

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

class procedure TMemTable.CopyMem(Source: Pointer; Dest: Pointer; Length: PtrUInt);
begin
  Move(Source^, Dest^, Length);
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

class procedure TMemTable.AddList(Source, Dest: TList);
var
  i : Integer;
begin
  for i := 0 to Source.Count -1 do
    Dest.Add(Source[i]);
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
    //Move(Pointer(PChar(Buffer) + SizeOf(LongWord))^, Pointer(Result)^, Len);
    CopyMem(Pointer(PChar(Buffer) + SizeOf(LongWord)), Pointer(Result), Len);
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
    //Move(Source^, Buffer^, Len + SizeOf(LongWord));
    CopyMem(Source, Buffer, Len + SizeOf(LongWord));
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

class function  TMemTable.Min(const A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

class function  TMemTable.Max(const A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

class function TMemTable.NewGuid(UseBrackets: Boolean): string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
  if not UseBrackets then
  begin
    if Result[1] = '{' then
      System.Delete(Result, 1, 1);
    if Result[Length(Result)] = '}' then
      System.Delete(Result, Length(Result), 1);
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
                      DT   := TimeStampToDateTime(TTimeStamp(Data1^));
                      DT2  := TimeStampToDateTime(TTimeStamp(Data2^));

                      Result := CompareDateTime(DT, DT2);

                    end;

    ftLargeint    : if Int64(Data1^) < Int64(Data2^) then
                      Result := -1
                    else if Int64(Data1^) > Int64(Data2^) then
                      Result := 1;

  end;
end;
(*----------------------------------------------------------------------------
  Compare functions should return
  -1  if Item1 < Item2,
   0  if Item1 = Item2,
   1  if Item1 > Item2

   IndexList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.CompareRecords(const RecBuf1, RecBuf2: PChar; const IndexFieldList: TList; Options: TLocateOptions; SortMode: TMemTableSortMode; BreakMode: TBreakMode): Integer;
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
      Data1   := RecBuf1 + FOffsets[Index];

      if Data1 <> nil then
      begin
        Data2    := RecBuf2 + FOffsets[Index];
        if Data2 <> nil then
        begin

          HasData1 := not IsFieldNull(RecBuf1, Index);
          HasData2 := not IsFieldNull(RecBuf2, Index);

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


  if (Result = 0) and (SortMode <> smNone) then
  begin
    if PRecInfo(RecBuf1 + FBookOfs)^.Bookmark  >  PRecInfo(RecBuf2 + FBookOfs)^.Bookmark then
      Result := 1
    else if PRecInfo(RecBuf1 + FBookOfs)^.Bookmark  <  PRecInfo(RecBuf2 + FBookOfs)^.Bookmark then
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

procedure TMemTable.LoadSortOnFieldList();
begin
  if Active or (FieldCount > 0) then
  begin
    FSortOnFields.Clear;
    GetFieldInfoList(FSortOnFields, FSortOnFieldNames);
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

  LoadSortOnFieldList();

  if (FSortOnFields.Count > 0) then
  begin
    FModes := FModes + [cmSort];
  end else begin
    FModes := FModes - [cmSort];
  end;

  Sort();
end;

procedure TMemTable.Sort();
var
  CurBuf : Pointer;
begin
  if Active and FInitialized and (cmSort in FModes) and (FRows.Count > 0) and (FSortOnFields.Count > 0) then
  begin
    if (FCurRecIndex < 0) or (FCurRecIndex >= FRows.Count) then
      CurBuf := nil
    else
      CurBuf := FRows[FCurRecIndex];

    QuickSort(0, FRows.Count - 1, FRows, FSortOnFields, [], SortMode);

    FCurRecIndex := FRows.IndexOf(CurBuf);

    UpdateCursorPos();
    Resync([]);
  end;
end;

procedure TMemTable.Sort(FieldNames: string; SortMode: TMemTableSortMode);
begin
  FSortMode := SortMode;
  SortOnFieldNames := FieldNames;
end;

procedure TMemTable.NextSort(FieldNames: string);
begin
  Sort(FieldNames, GetNextSortMode());
end;

function TMemTable.GetNextSortMode() : TMemTableSortMode;
begin
  if (FSortMode = smNone) then
    Result := smAsc
  else if (FSortMode = smAsc) then
    Result := smDesc
  else
    Result := smNone;
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

procedure TMemTable.SetRange(const RangeFieldNames: string; const StartValues, EndValues: Variant; Exclusive: Boolean);
begin
  CheckBrowseMode;

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

procedure TMemTable.VariantValuesToRecordBuffer(FieldList: TList; RecBuf: PChar; Values: Variant);
var
  i         : Integer;
  FieldInfo : TFieldInfo;
  Value     : Variant;
  Index     : Integer;
begin
  { populate the locate record buffer }
  InternalInitRecord(RecBuf);

  for i := 0 to FieldList.Count - 1 do
  begin
    FieldInfo := TFieldInfo(FieldList[i]);

    if (FieldList.Count = 1) and not VarIsArray(Values) then
      Value := Values
    else
      Value := Values[i];

    if not VarIsNull(Value) then
    begin
      Index   := FieldInfo.Index;
      SetValueToRecBuf(RecBuf, Index, Value);
    end;
  end;

end;

function TMemTable.GetValueFromRecBuf(RecBuf: Pointer; const FieldIndex: Integer): Variant;
var
  FieldInfo: TFieldInfo;
  Field    : TField;
  FieldBuf : Pointer;
begin
   Result := Null;

  if not IsFieldNull(RecBuf, FieldIndex) then
  begin
    FieldInfo := GetFieldInfo(FieldIndex);
    Field     := FieldInfo.FField;;

    FieldBuf  := AllocMem(FFieldBufferSizes[FieldIndex]);
    try
      Field.GetData(FieldBuf);
      Result := GetValueFromFieldBuf(FieldBuf, FieldIndex)
    finally
      FreeMem(FieldBuf);
    end;
  end;

end;
procedure TMemTable.SetValueToRecBuf(RecBuf: Pointer; const FieldIndex: Integer; const Value: Variant);
var
  FieldBuf            : Pointer;
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
function TMemTable.GetValueFromFieldBuf(FieldBuf: Pointer; const FieldIndex: Integer): Variant;
var
  S                 : string;
  WS                : WideString;
  CY                : Currency;
begin
  Result := Null;

  case FFieldTypes[FieldIndex] of
    ftString         ,
    ftFixedChar      ,
    ftGuid           : begin
                         S := String(PChar(FieldBuf));
                         Result := S;
                       end;

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

    ftTimeStamp      : Result  := TimeStampToMSecs(TTimeStamp(FieldBuf^));

    ftMemo           : ;
    ftWideMemo       : ;
    ftFmtMemo        : ;

    ftBlob           : ;
    ftGraphic        : ;

    ftOraBlob        : ;
    ftOraClob        : ;

  end;

end;
(*----------------------------------------------------------------------------
 FieldData is the Field's data buffer, null flag byte NOT included.
 This method does set any null flag byte.
 The caller is responsible for setting that flag, IF FieldData comes from a record buffer.
 ----------------------------------------------------------------------------*)
procedure TMemTable.SetValueToFieldBuf(FieldBuf: Pointer; const FieldIndex: Integer; Value: Variant);
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
                                 //Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                                 CopyMem(P, FieldBuf, FFieldBufferSizes[FieldIndex]);
                               end;

    ftDate                   ,
    ftTime                   ,
    ftDateTime               : begin
                                 DTR := DateTimeToNative(FFieldTypes[FieldIndex], VarToDateTime(Value));
                                 P   := @DTR;
                                 //Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                                 CopyMem(P, FieldBuf, FFieldBufferSizes[FieldIndex]);
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
                                 //Move(P^, FieldBuf^, FFieldBufferSizes[FieldIndex]);
                                 CopyMem(P, FieldBuf, FFieldBufferSizes[FieldIndex]);
                                 Value := Null;
                               end;

  end;

end;

function TMemTable.GetRecordByIndex(RecBuf: Pointer; RecordIndex: Integer): Boolean;
var
  SourceRecBuf : PChar;
begin
  if FInitialized and (RecordIndex >= 0) and (RecordIndex <= FRows.Count - 1) then
    SourceRecBuf := FRows[RecordIndex]
  else
    SourceRecBuf := nil;

  Result := Assigned(SourceRecBuf);

  if Result then
    CopyMem(SourceRecBuf, RecBuf, FRecBufSize);

end;
(*----------------------------------------------------------------------------
  IndexFieldList is a TList containing TFieldInfo objects
 ----------------------------------------------------------------------------*)
function TMemTable.LocateRecord(const IndexFieldNames: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean;  var RecIndex: Integer): Boolean;
var
  i                 : Integer;
  IndexList         : TList;
begin
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

procedure TMemTable.OnFilterVariableValueEvent(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
var
  FieldInfo: TFieldInfo;
  FieldIndex: Integer;
begin
  FieldInfo  := GetFieldInfo(Variable);

  if not Assigned(FieldInfo) then
    FFilterParser.Error(Format('Field not found: %s', [Variable]));

  FieldIndex := FieldInfo.Index;

  Value := GetValueFromRecBuf(ClientTag, FieldIndex);
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

function TMemTable.FilterCanDisplayRecord(RecBuf: Pointer): Boolean;
var
  V : Variant;
begin
  V := FFilterParser.Evaluate(RecBuf);
  Result := VarIsBool(V) and (V = True);
end;

function  TMemTable.GetEncoding(): string;
begin
  if Length(FEncoding) = 0 then
    Result := 'utf-8'
  else
    Result := FEncoding;
end;

function TMemTable.ToXmlText(SchemaOnly: Boolean): string;
begin
  Result := XmlPersistor.ToXmlText(Self, SchemaOnly);
end;

procedure TMemTable.SaveToXmlFile(const FileName: string; SchemaOnly: Boolean);
begin
  XmlPersistor.SaveToXmlFile(Self, FileName, SchemaOnly);
end;

procedure TMemTable.SaveToXmlStream(Stream: TStream; SchemaOnly: Boolean);
begin
  XmlPersistor.SaveToXmlStream(Self, Stream, SchemaOnly);
end;

procedure TMemTable.FromXmlText(XmlText: string; SchemaOnly: Boolean);
var
  Old: Boolean;
begin
  Old := AutoIncEnabled;
  AutoIncEnabled := False;
  try
    XmlPersistor.FromXmlText(Self, XmlText, SchemaOnly);
  finally
    AutoIncEnabled := Old;
  end;
end;

procedure TMemTable.LoadFromXmlFile(const FileName: string; SchemaOnly: Boolean);
begin
  XmlPersistor.LoadFromXmlFile(Self, FileName, SchemaOnly);
end;

procedure TMemTable.LoadFromXmlStream(Stream: TStream; SchemaOnly: Boolean);
begin
  XmlPersistor.LoadFromXmlStream(Self, Stream, SchemaOnly);
end;

end.

