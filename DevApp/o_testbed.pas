unit o_TestBed;

{$mode ObjFPC}{$H+}

interface

uses
    Classes
    , SysUtils
    , FileUtil
    , Forms
    , Controls
    , Graphics
    , StdCtrls
    , Dialogs
    , Menus
    , DBGrids
    , TypInfo
    , Variants
    //, syncobjs
    , Tripous
    , Tripous.Data
    , LazFileUtils
    , DB
    , SQLDB
    , SQLTypes
    , Generics.Collections
    //, csvdataset
    ,Laz2_DOM, RTTIGrids
    //,laz2_XMLWrite
    ,Rtti
  ;


type
  generic IBox<T> = interface
  ['{1420230C-41B4-434D-BB96-BF40341912A1}']
    function  GetValue(): T;
    procedure SetValue(V: T);

    property Value: T read GetValue write SetValue;
  end;

  { TBox }
  generic TBox<T> = class(TInterfacedObject, specialize IBox<T>)
  private
    FOwnsObject: Boolean;
    FValue: T;
  public
    constructor Create(Value: T; OwnsObject: Boolean = False);
    destructor Destroy(); override;

    function  GetValue(): T;
    procedure SetValue(V: T);

    property Value: T read GetValue write SetValue;
    property OwnsObject: Boolean read FOwnsObject;
  end;

  IObjectBox = specialize IBox<TObject>;
  TObjectBox = specialize TBox<TObject>;

procedure DeStreamTest();
procedure TestSqlConnectionInfo();
procedure TestJson();
function TestSchemaInfo(): string;
procedure TestMetastores();


implementation

uses
    fpjson
    ,jsonscanner
    //,jsonparser
    ,fpjsonrtti
    ;

procedure XmlTest();
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  S : string;
begin

    Doc := Xml.CreateDoc();
    Node := Xml.AddNode(Doc.DocumentElement, 'First', True);

    S := 'Ολα καλά' + LB + 'ki ola wraia';
    Xml.SetCData(Doc.DocumentElement, 'Second', S);

    //Xml.SetChild(Doc.DocumentElement, 'First', 'Paparia');
    //S := Xml.GetNodeValue(Node, '');
    //ShowMessage(S);

    //TDOMElement(Node)['FirstName'] := 'Θόδωρος';
    //S := TDOMElement(Node)['FirstName'];
    //ShowMessage(S);

    Xml.SaveToFile(Doc, 'Test.Xml');

    S := Xml.DocToText(Doc);
    ShowMessage(S);
end;



type
  TNameObject = class(TCollectionItem) // class for the 'obj' property and TCollection
  private
    fName: String;
  published
    property name: String read fName write fName;
  end;

  TBaseObject = class(TPersistent)  // class for the entire JSON structure
  private
    fid: Integer;
    fObj: TNameObject;
    fColl: TCollection;
    fStrings: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
  published                         // all properties must be published
    property id: Integer read fid write fid;
    property obj: TNameObject read fObj write fObj;
    property Coll: TCollection read fColl;
    property strings: TStrings read fStrings;
  end;

  { TBox }

constructor TBox.Create(Value: T; OwnsObject: Boolean);
begin
  inherited Create();
  Self.Value := Value;
  FOwnsObject := OwnsObject;
end;

destructor TBox.Destroy();
begin
  if (OwnsObject) then
  try
     TObject(T).Free();   // exception here
  except
  end;

  inherited Destroy();
end;

function TBox.GetValue(): T;
begin
  Result := FValue;
end;

procedure TBox.SetValue(V: T);
begin
  FValue := V;
end;

  constructor TBaseObject.Create;
  begin
    // Create Collection and StringList
    fColl    := TCollection.Create(TNameObject);
    fStrings := TStringList.Create;
    fObj     := TNameObject.Create(nil);
  end;

  destructor TBaseObject.Destroy;
  begin
    // Release Collection and StringList
    fColl.Free;
    fStrings.Free;
    fObj.Free;
    inherited Destroy;
  end;


procedure DeStreamTest();
var
  DeStreamer: TJSONDeStreamer;
  o: TBaseObject;
  no: TNameObject;
  s, s2: String;
  JsonText: string;
  Count: Integer;
begin
  JsonText :=
  '{                                                               ' +
  '  "id"     : 123,                                               ' +
  '  "obj"    : { "name": "Hello world!" },                        ' +
  '  "coll"   : [ { "name": "Object 1" }, { "name": "Object 2" } ],' +
  '  "strings": [ "Hello 1", "Hello 2" ]                           ' +
  '}                                                               '
;

  o := TBaseObject.Create;
  try

    DeStreamer := TJSONDeStreamer.Create(nil);
    DeStreamer.Options :=  [jdoCaseInsensitive, jdoIgnoreNulls] ;
    DeStreamer.JSONToObject(JsonText, o);
    DeStreamer.Destroy;

    Count := o.strings.Count;
    Count := o.coll.Count;

    // output the names of all objects
    for TCollectionItem(no) in o.coll do
     s2 := no.name;

    //for s in o.strings do
    //  WriteLn(s);

  finally
    o.Destroy;
  end;

end;


procedure TestSqlConnectionInfo();
var
   SqlConInfoList: TSqlConnectionInfoList;
   DeStreamer: TJSONDeStreamer;
   Count: Integer;
   JsonStr: TJSONStringType;
begin
  JsonStr :=
'{                                      ' +
'  "SqlConnections" : [                 ' +
'    {                                  ' +
'      "AutoCreateGenerators" : false,  ' +
'      "ConnectionString" : "",         ' +
'      "Name" : "Default",              ' +
'      "Provider" : "MsSql"             ' +
'    },                                 ' +
'    {                                  ' +
'      "AutoCreateGenerators" : false,  ' +
'      "ConnectionString" : "",         ' +
'      "Name" : "Con2",                 ' +
'      "Provider" : "Firebird"          ' +
'    }                                  ' +
'  ]                                    ' +
'}                                      '
 ;
  SqlConInfoList := TSqlConnectionInfoList.Create();

  DeStreamer := TJSONDeStreamer.Create(nil);
  //DeStreamer.Options :=  [jdoCaseInsensitive, jdoIgnoreNulls];
  DeStreamer.JSONToObject(JsonStr, SqlConInfoList);
  DeStreamer.Destroy;

  Count := SqlConInfoList.Count;

end;

type

  { TMan }
  TMan = class(TPersistent)
  private
    FName: string;
  published
    property Name : string read FName write FName;
  end;

procedure TestJson();
var
   M : TMan;
   JsonText: string;
begin
   M := TMan.Create();
   M.Name := 'Teo';

   JsonText := Json.Serialize(M);

   M := TMan.Create();
   Json.Deserialize(M, JsonText);


end;

type
  TSqlObjectIdenfierClass = class of TSqlObjectIdenfier;

function TestSchemaInfo(): string;
var
   Con : TSQLConnector;
   SchemaList: TSqlObjectIdentifierList;
   Trans: TSQLTransaction;
   List : TStringList;
   i : Integer;
   Item : TSqlObjectIdenfier;
begin
  // TSchemaType = (stNoSchema, stTables, stSysTables, stProcedures, stColumns, stProcedureParams, stIndexes, stPackages, stSchemata, stSequences);
  // function GetObjectNames(ASchemaType: TSchemaType; AList : TSqlObjectIdentifierList): Integer; virtual;

  Result := '';

  List := TStringList.Create();
  Con := TSQLConnector.Create(nil);
  Trans := TSQLTransaction.Create(Con);
  Con.Transaction := Trans;
  SchemaList := TSqlObjectIdentifierList.Create(TSqlObjectIdenfier);
  try
    Con.UserName := 'SYSDBA';
    Con.Password := 'mirodato';
    Con.ConnectorType := 'Firebird';
    Con.DatabaseName := 'C:\Program Files\Firebird\Firebird_5_0\examples\empbuild\EMPLOYEE.FDB';
    Con.Open();
    //Trans.Active:= True;

    // stTables
    // stSysTables   ??
    Con.GetObjectNames(stTables, SchemaList);
    for i := 0 to SchemaList.Count - 1 do
    begin
      Item := TSqlObjectIdenfier(SchemaList[i]);
      List.Add(Trim(Item.ObjectName) + ' -- ' + Trim(Item.FullName));
    end;

    Result := List.Text;
  finally
    //Trans.Active:= False;
    Con.Close();

    SchemaList.Free();
    Trans.Free();
    Con.Free();
    List.Free();
  end;

end;

procedure TestMetastores();
var
  ConInfo: TSqlConnectionInfo;
begin
  ConInfo := TSqlConnectionInfo.Create(nil);
  try
    ConInfo.Name := 'Default';
    ConInfo.Provider := SqlProviders.ProviderTypeToString(ptFirebird);
    ConInfo.ConnectionString := 'User = SYSDBA; Psw = mirodato; Database = C:\Program Files\Firebird\Firebird_5_0\examples\empbuild\EMPLOYEE.FDB';

    DbSys.MetaDatabases.Add(ConInfo);
  finally
    ConInfo.Free();
  end;


end;

end.

