unit Tripous.Data;

{$mode objfpc}{$H+}

interface

uses
   Classes
  ,SysUtils
  ,Variants
  ,DB
  ,bufdataset
  ,sqldb

  ,Tripous
  ;

type
   TSetOfFieldType    = set of TFieldType;

   { TSqlConnectionInfo }
   TSqlConnectionInfo = class(TCollectionItem)
   private
     FName: string;
     FProvider: string;
   public
     constructor Create();
   published
     property Name: string read FName write FName;
     property Provider: string read FProvider write FProvider;
   end;

   { TSqlConnectionInfoList }
   TSqlConnectionInfoList = class(TCollection)
   public
     constructor Create();
     procedure Add(Item: TSqlConnectionInfo);
   end;

  { DbSys }
  DbSys = class
  private
    class function GetBCDFieldTypes: TSetOfFieldType; static;
    class function GetBlobFieldTypess: TSetOfFieldType; static;
    class function GetDateTimeFieldTypes: TSetOfFieldType; static;
    class function GetFloatFieldTypes: TSetOfFieldType; static;
    class function GetIntegerFieldTypes: TSetOfFieldType; static;
    class function GetStringFieldTypes: TSetOfFieldType; static;
    class function GetWideStringFieldTypes: TSetOfFieldType; static;
  public
    { construction }
    class constructor Create();
    class destructor Destroy();

    class function  AsInteger(Table: TDataset; const FieldName: string; Default: Integer = -1): Integer;
    class function  AsFloat(Table: TDataset; const FieldName: string; Default: Double = 0): Double;
    class function  AsFloatString(Table: TDataset; const FieldName: string; Digits: Integer = 3; Default: string = ''): string;
    class function  AsString(Table: TDataset; const FieldName: string; Default: string = ''): string;
    class function  AsBoolean(Table: TDataset; const FieldName: string; Default: Boolean = False): Boolean;
    class function  AsDateTime(Table: TDataset; const FieldName: string; Default: TDateTime = 0): TDateTime;
    class procedure SafeSetFieldValue(Table: TDataset; FieldName: string; Value: Variant);

    class function  CreateField(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
    class function  CreateFieldLookUp(Dataset: TDataset; const FieldName, KeyFields: string; LookupDataSet: TDataset; LookupKeyFields, LookupResultField: string): TField;
    class function  CreateFieldInternalCalc(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
    class function  AddFieldDef(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; Required: Boolean = False): TFieldDef;

    class procedure CopyFieldStructure(SourceField: TField; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
    class procedure CopyDatasetStructure(Source: TDataset; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
    class procedure MergeStructure(Source, Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);

    class procedure CopyField(SourceField, DestField: TField);
    class procedure CopyFieldBlob(SourceField, DestField: TBlobField);
    class procedure CopyRecord(Source, Dest: TDataset);
    class procedure CopyRecordAppend(Source, Dest: TDataset);
    class procedure CopyDataset(Source, Dest: TDataset);
    class procedure EmptyDataset(Dataset: TDataset);
    class function  IdenticalSchema(Table1, Table2: TDataset): Boolean;

    class procedure StrToBlob(Field: TBlobField; const Data: string);
    class function  BlobToStr(Field: TBlobField): string;

    class procedure StreamToBlob(Field: TBlobField; Stream: TStream);
    class function  BlobToStream(Field: TBlobField): TStream;

    class function  FieldValuesToVariantArray(Dataset: TDataset;  const ResultFields: string): Variant;
    class procedure GetKeyValuesList(List: TStrings; Table: TDataset; const FieldName: string; ModValue: Integer; DiscardBelowZeroes: Boolean);

    class procedure ParamsAssign(Params: TParams; tblParams: TDataset); overload;

    { properties }
    class property StringFieldTypes  : TSetOfFieldType read GetStringFieldTypes;
    class property WideStringFieldTypes : TSetOfFieldType read GetWideStringFieldTypes;
    class property IntegerFieldTypes : TSetOfFieldType read GetIntegerFieldTypes;
    class property FloatFieldTypes : TSetOfFieldType read GetFloatFieldTypes;
    class property BCDFieldTypes : TSetOfFieldType read GetBCDFieldTypes;
    class property DateTimeFieldTypes : TSetOfFieldType read GetDateTimeFieldTypes;
    class property BlobFieldTypes  : TSetOfFieldType read GetBlobFieldTypess;
  end;



implementation

{ TSqlConnectionInfo }

constructor TSqlConnectionInfo.Create();
begin
  inherited Create(nil);
end;

{ TSqlConnectionInfoList }

constructor TSqlConnectionInfoList.Create();
begin
  inherited Create(TSqlConnectionInfo);
end;

procedure TSqlConnectionInfoList.Add(Item: TSqlConnectionInfo);
begin
  Item.Collection := Self;
end;





{ DbSys }



class constructor DbSys.Create();
begin
end;

class destructor DbSys.Destroy();
begin
end;





class function DbSys.GetStringFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftString
  ,ftGuid
  ,ftFixedChar
  ];
end;

class function DbSys.GetWideStringFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftWideString
  ,ftFixedWideChar
  ];
end;

class function DbSys.GetIntegerFieldTypes: TSetOfFieldType; static;
begin
   Result :=
  [ftAutoInc
  ,ftSmallint
  ,ftInteger
  ,ftWord
  ,ftLargeint
  ];
end;

class function DbSys.GetFloatFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftFloat
  ,ftCurrency
  ];
end;

class function DbSys.GetBCDFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftBCD
  ,ftFMTBcd
  ];
end;

class function DbSys.GetDateTimeFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftDate
  ,ftTime
  ,ftDateTime
  ,ftTimeStamp
  ];
end;

class function DbSys.GetBlobFieldTypess: TSetOfFieldType; static;
begin
  Result :=
  [ftMemo
  ,ftWideMemo
  ,ftFmtMemo

  ,ftBlob
  ,ftGraphic

  ,ftOraBlob
  ,ftOraClob
  ];
end;






(*----------------------------------------------------------------------------*)
class function DbSys.AsInteger(Table: TDataset; const FieldName: string; Default: Integer = -1): Integer;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsInteger
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsFloat(Table: TDataset; const FieldName: string; Default: Double = 0): Double;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsFloat
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsFloatString(Table: TDataset; const FieldName: string; Digits: Integer = 3; Default: string = ''): string;
var
  Field : TField;
begin

  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Sys.CommaToDot(Sys.DoubleToStr(Field.AsFloat , Digits))
  else
    Result := Default;

end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsString(Table: TDataset; const FieldName: string; Default: string = ''): string;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsString
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsBoolean(Table: TDataset; const FieldName: string; Default: Boolean = False): Boolean;
var
  Field : TField;
begin
  Result := Default;
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
  begin
    if (Field.DataType in IntegerFieldTypes) or (Field.DataType in FloatFieldTypes) then
      Result := Boolean(Integer(Field.AsInteger))
    else if (Field.DataType = ftBoolean) then
      Result := Field.AsBoolean
    else if (Field.DataType in StringFieldTypes) then
    begin
      if not TryStrToBool(Field.AsString, Result) then
        Result := Default;
    end
  end;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsDateTime(Table: TDataset; const FieldName: string; Default: TDateTime = 0): TDateTime;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsDateTime
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.SafeSetFieldValue(Table: TDataset; FieldName: string; Value: Variant);
begin
  if Table.FindField(FieldName) <> nil then
  begin
    if not (Table.State in [dsEdit, dsInsert]) then
      Table.Edit;
    Table.FieldByName(FieldName).Value := Value;
  end;
end;
(*----------------------------------------------------------------------------*)
class function  DbSys.CreateField(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
var
  Owner: TComponent;
begin
  if Assigned(Dataset.Owner) then
    Owner := Dataset.Owner
  else
    Owner := Dataset;

  Result := DefaultFieldClasses[DataType].Create(Owner);

  Result.FieldName := FieldName;
  if Size > 0 then
    Result.Size      := Size;

  Result.DataSet := Dataset;

  if DisplayLabel <> '' then
    Result.DisplayLabel := DisplayLabel;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.CreateFieldInternalCalc(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer; DisplayLabel: string): TField;
begin
  Result := CreateField(Dataset, FieldName, DataType, Size, DisplayLabel);
  Result.FieldKind := fkInternalCalc;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.CreateFieldLookUp(Dataset: TDataset; const FieldName, KeyFields: string; LookupDataSet: TDataset; LookupKeyFields, LookupResultField: string): TField;
var
  Field      : TField;
  FieldClass : TFieldClass;
begin
  Field := LookupDataSet.FindField(LookupResultField);
  if not Assigned(Field) then
    raise Exception.CreateFmt('CreateFieldLookUp'#13'Field %s not found in LookUp Table', [LookupResultField]);

  FieldClass := TFieldClass(Field.ClassType);

  Field := FieldClass.Create(Dataset);
  Field.FieldName          := FieldName;
  Field.FieldKind          := fkLookup;
  Field.KeyFields          := KeyFields;
  Field.DataSet            := Dataset;

  Field.LookupKeyFields    := LookupKeyFields;
  Field.LookupResultField  := LookupResultField;
  Field.LookupDataSet      := LookupDataSet;

  //if Assigned(FieldList) then
    //FieldList.Add(Field);

  Result := Field;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AddFieldDef(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer; Required: Boolean): TFieldDef;
begin
  Result          := Dataset.FieldDefs.AddFieldDef();
  Result.Name     := FieldName;
  Result.DataType := DataType;
  Result.Size     := Size;
  Result.Required := Required;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyFieldStructure(SourceField: TField; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  TargetField : TField;
  FieldClass  : TFieldClass;
begin
  FieldClass  := TFieldClass(SourceField.ClassType);
  if (FieldClass.InheritsFrom(TAutoIncField)) and AutoIncToInt then
    FieldClass := TIntegerField;

  TargetField := FieldClass.Create(Target);

  TargetField.Size              := SourceField.Size;
  TargetField.FieldKind         := SourceField.FieldKind;
  TargetField.FieldName         := SourceField.FieldName;
  TargetField.DisplayLabel      := SourceField.DisplayLabel;
  TargetField.Visible           := SourceField.Visible;

  if SetRequired then
    TargetField.Required := SourceField.Required;

  //TargetField.Lookup            := SourceField.Lookup;
  TargetField.KeyFields         := SourceField.KeyFields;
  TargetField.LookupDataSet     := SourceField.LookupDataSet;
  TargetField.LookupResultField := SourceField.LookupResultField;
  TargetField.LookupKeyFields   := SourceField.LookupKeyFields;

  if SourceField is TBCDField then
    TBCDField(TargetField).Precision := TBCDField(SourceField).Precision;

  TargetField.DataSet      := Target;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyDatasetStructure(Source: TDataset; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  i : Integer;
begin
  Target.Active := False;
  Target.Fields.Clear;

  for i := 0 to Source.FieldCount - 1 do
    CopyFieldStructure(Source.Fields[i], Target, SetRequired, AutoIncToInt);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.MergeStructure(Source, Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  i : Integer;
begin
  for i := 0 to Source.FieldCount - 1 do
    if Target.FindField(Source.Fields[i].FieldName) = nil then
      CopyFieldStructure(Source.Fields[i], Target, SetRequired, AutoIncToInt);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyField(SourceField, DestField: TField);
begin
  if Assigned(SourceField) and Assigned(DestField) then
  begin
    if SourceField.IsNull then
      DestField.Clear
    else if (SourceField is TLargeintField) or (DestField is TLargeintField) then
      DestField.AsInteger := SourceField.AsInteger
    else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
      CopyFieldBlob(TBlobField(SourceField), TBlobField(DestField))
    else
      DestField.Value := SourceField.Value;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyFieldBlob(SourceField, DestField: TBlobField);
var
  MS : TMemoryStream;
begin
  if Assigned(SourceField) and Assigned(DestField) then
  begin
    MS := TMemoryStream.Create;
    try
      SourceField.SaveToStream(MS);
      MS.Position := 0;
      DestField.LoadFromStream(MS);
    finally
      MS.Free;
    end;
  end;

end;
(*----------------------------------------------------------------------------
  Source and Dest datasets may not be identical in schema
 ----------------------------------------------------------------------------*)
class procedure DbSys.CopyRecord(Source, Dest: TDataset);
var
  i            : Integer;
  SourceField  : TField;
  DestField    : TField;
  MS           : TMemoryStream;
begin
  MS         := TMemoryStream.Create;
  try
    Dest.Edit;

    for i := 0 to Source.FieldCount - 1 do
    begin
      SourceField  := Source.Fields[i];
      DestField    := Dest.FindField(Source.Fields[i].FieldName);
      if (not SourceField.IsNull) and Assigned(DestField) then
      begin
        if (SourceField is TLargeintField) or (DestField is TLargeintField) then
            DestField.AsInteger := SourceField.AsInteger
        else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
        begin
          MS.Clear;
          TBlobField(SourceField).SaveToStream(MS);
          MS.Position := 0;
          TBlobField(DestField).LoadFromStream(MS);
        end else
          DestField.Value := SourceField.Value;
      end;
    end;

    Dest.Post;
  finally
    MS.Free;
  end;

end;
(*----------------------------------------------------------------------------
  Source and Dest datasets may not be identical in schema
 ----------------------------------------------------------------------------*)
class procedure DbSys.CopyRecordAppend(Source, Dest: TDataset);
begin
  Dest.Append;
  CopyRecord(Source, Dest);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyDataset(Source, Dest: TDataset);
var
  BM         : TBookMark;
  SourceList : TList;
  DestList   : TList;
  i          : Integer;
  Field      : TField;
  SourceField: TField;
  DestField  : TField;
  MS         : TMemoryStream;
begin
  BM := Source.Bookmark;
  Source.DisableControls;
  Dest.DisableControls;
  SourceList := TList.Create;
  DestList   := TList.Create;
  MS         := TMemoryStream.Create;
  try

    for i := 0 to Source.FieldCount - 1 do
    begin
      Field := Dest.FindField(Source.Fields[i].FieldName);
      if Assigned(Field) then
      begin
        SourceList.Add(Source.Fields[i]);
        DestList.Add(Field);
      end;
    end;

    Source.First;
    while not Source.Eof do
    begin

      Dest.Append;

      for i := 0 to SourceList.Count - 1 do
      begin
        SourceField := TField(SourceList[i]);
        DestField   := TField(DestList[i]);
        if (not SourceField.IsNull) then
        begin
          if (SourceField is TLargeintField) or (DestField is TLargeintField) then
            DestField.AsInteger := SourceField.AsInteger
          else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
          begin
            MS.Clear;
            TBlobField(SourceField).SaveToStream(MS);
            MS.Position := 0;
            TBlobField(DestField).LoadFromStream(MS);
          end else
            //SourceField.AssignValue(DestField.Value);
            DestField.Value := SourceField.Value;
        end;
      end;

      Dest.Post;

      Source.Next;
    end;

    if not Dest.IsEmpty then
      Dest.First;

  finally
    MS.Free;
    DestList.Free;
    SourceList.Free;
    Dest.EnableControls;
    Source.Bookmark := BM;
    Source.EnableControls;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.EmptyDataset(Dataset: TDataset);
begin
  if Assigned(Dataset) and (Dataset.Active) and (not Dataset.IsEmpty) then
  begin
    Dataset.DisableControls;
    try
      Dataset.First;
      while not Dataset.Eof do
        Dataset.Delete;
    finally
      Dataset.EnableControls;
    end;
  end;

end;
(*----------------------------------------------------------------------------*)
class function  DbSys.IdenticalSchema(Table1, Table2: TDataset): Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Table1.FieldCount - 1 do
    if not (AnsiSameText(Table1.Fields[i].FieldName, Table2.Fields[i].FieldName) and (Table1.Fields[i].FieldKind = Table2.Fields[i].FieldKind)) then
    begin
      Result := False;
      Exit; //==>
    end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.StrToBlob(Field: TBlobField; const Data: string);
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer(PChar(Data)^, Length(Data));
    MS.Position := 0;
    Field.Assign(nil);
    Field.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.BlobToStr(Field: TBlobField): string;
var
  MS: TMemoryStream;
begin
  Result := '';
  MS := TMemoryStream.Create;
  try
    Field.SaveToStream(MS);
    MS.Position := 0;
    SetLength(Result, MS.Size);
    MS.ReadBuffer(PChar(Result)^, MS.Size);
  finally
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.StreamToBlob(Field: TBlobField; Stream: TStream);
begin
  Field.Assign(nil);
  Field.LoadFromStream(Stream);
end;
(*----------------------------------------------------------------------------*)
class function  DbSys.BlobToStream(Field: TBlobField): TStream;
begin
  Result := TMemoryStream.Create;
  Field.SaveToStream(Result);
  Result.Position := 0;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.FieldValuesToVariantArray(Dataset: TDataset;  const ResultFields: string): Variant;
var
  i    : Integer;
  List : TStringList;
begin
  Result := Variants.Null;
  List := TStringList.Create;
  try
   Sys.Split(ResultFields, ';', List);

   { we're going to return the value of a SINGLE field }
   if List.Count = 1 then
   begin
     if (Dataset.FindField(List[0]) <> nil) then
       Result := Dataset.Fields[0].Value
     else
       Result := Variants.Null;

   { we're going to return values more than one field }
   end else if List.Count > 1 then
   begin
     Result := VarArrayCreate([0, List.Count - 1], varVariant);
     for i := 0 to List.Count - 1 do
       if (Dataset.FindField(List[i]) <> nil) then
         Result[i] := Dataset.Fields[i].Value
       else
         Result[i] := Variants.Null;
   end;
  finally
    List.Free;
  end;
end;
(*----------------------------------------------------------------------------
  for creating statements like

    select
      *
    from
      TABLE_NAME
    where
      FIELD_NAME in (...)

  in order to avoid limit problems many servers, like Oracle,
  impose, for the in set of values.
 ----------------------------------------------------------------------------*)
class procedure DbSys.GetKeyValuesList(List: TStrings; Table: TDataset; const FieldName: string; ModValue: Integer; DiscardBelowZeroes: Boolean);
var
  Counter   : Integer;
  S         : string;
  Field     : TField;
  i         : Integer;
  IsString  : Boolean;
begin
  List.Clear;

  Field := Table.FindField(FieldName);
  if not Assigned(Field) then
    Exit; //==>

  IsString := (Field is TStringField);

  Counter := 0;
  S       := '';


  Table.DisableControls;
  try
    Table.First;
    while not Table.Eof do
    begin
      if (not Field.IsNull) then
      begin
        case IsString of
          False : if (DiscardBelowZeroes and (Field.AsInteger > 0)) or (not DiscardBelowZeroes) then
                  begin
                    S  := S + Field.AsString + ', ';
                    Inc(Counter);
                  end;
          True  : begin
                    S  := S + Sys.QS(Field.AsString) + ', ';
                    Inc(Counter);
                  end;
        end;

        if (Counter mod ModValue = 0) then
        begin
          List.Add(S);
          S := '';
        end;

      end;
      Table.Next;
    end;
  finally
    Table.EnableControls;
  end;


  if S <> '' then
    List.Add(S);

  for i := 0 to List.Count - 1 do
  begin
    S := List[i];
    if Length(S) > 2 then
      SetLength(S, Length(S) - 2);
    List[i] := Format('(%s)', [S]);
  end;

end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.ParamsAssign(Params: TParams; tblParams: TDataset);
var
  i        : Integer;
  Field    : TField;
  Param    : TParam;
  MS       : TMemoryStream;
begin
  if not Assigned(tblParams) then
    Exit; //==>

  for i := 0 to Params.Count - 1 do
  begin
    Param := Params[i];
    Field := tblParams.FindField(Param.Name);

    if Assigned(Field) then
    begin
      if Field.IsNull then
        Param.Value := Variants.Null
      else if (Field.DataType in BlobFieldTypes) then
      begin
        MS := TMemoryStream.Create;
        try
          TBlobField(Field).SaveToStream(MS);
          MS.Position := 0;
          if Param.DataType = ftGraphic then
            Param.LoadFromStream(MS, ftGraphic)
          else Param.LoadFromStream(MS, ftBlob);
        finally
          MS.Free;
        end;
      end else begin
        //Param.AssignField(Field);
        Param.Value := Field.Value;
      end;
    end;
  end;

end;














initialization
  //

end.

