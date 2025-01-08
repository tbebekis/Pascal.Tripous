unit o_TestBed;
                // ObjFPC
{$mode DELPHI}{$H+}
{$WARN 5079 off : Unit "$1" is experimental}
{$WARN 5027 off : Local variable "$1" is assigned but never used}
interface

uses
    Classes
    , SysUtils
    , FileUtil
    , Forms
    , Controls
    , Graphics
    //, StdCtrls
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
    //, Generics.Collections
    //, csvdataset
    ,Laz2_DOM, RTTIGrids
    //,laz2_XMLWrite
    ,Rtti
    ,LCLType
  ;





procedure TestSqlConnectionInfo();
function  TestSchemaInfo(): string;
procedure TestMetastores();

procedure TestDynArray();

function LoadResourceAsString(ResourceName: string): string;


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

  procedure FirebirdConInfo(ConInfo: TSqlConnectionInfo);
  begin
    ConInfo.Provider := SqlProviders.ProviderTypeToString(ptFirebird);
    ConInfo.ConnectionString := 'Server=localhost; User=SYSDBA; Psw=mirodato; Database=C:\Program Files\Firebird\Firebird_5_0\examples\empbuild\EMPLOYEE.FDB';
  end;

  procedure MsSqlConInfo(ConInfo: TSqlConnectionInfo);
  begin
    ConInfo.Provider := SqlProviders.ProviderTypeToString(ptMsSql);
    ConInfo.ConnectionString := 'Server=localhost; Database=AdventureWorksLT2012; Integrated Security=SSPI';
  end;

  procedure MySqlConInfo(ConInfo: TSqlConnectionInfo);
  begin
    ConInfo.Provider := SqlProviders.ProviderTypeToString(ptMySql);
    ConInfo.ConnectionString := 'Server=localhost; Database=employees; User=root; Psw=yoda2k';
  end;

var
  ConInfo: TSqlConnectionInfo;
begin
  ConInfo := TSqlConnectionInfo.Create(nil);
  try
    ConInfo.Name := 'Default';

    //FirebirdConInfo(ConInfo);
    //MsSqlConInfo(ConInfo);
    MySqlConInfo(ConInfo);

    DbSys.MetaDatabases.Add(ConInfo);
  finally
    ConInfo.Free();
  end;

  DbSys.MetaDatabases.Clear();
end;

type

  { TPerson }

  TPerson = class
  private
    FName: string;
  public
    constructor Create(AName: string);
    destructor Destroy(); override;

    property Name: string read FName write FName;
  end;

{ TPerson }

constructor TPerson.Create(AName: string);
begin
  inherited Create();
  FName := AName;
end;

destructor TPerson.Destroy();
var
  S : string;
begin
  S := FName;
  inherited Destroy();
end;

procedure TestDynArray();
var
  A: TGenObjectList<TPerson>;
begin
  A := TGenObjectList<TPerson>.Create(True, True);

  A.Add(TPerson.Create('teo'));
  A.Add(TPerson.Create('lakis'));

  A.RemoveAt(0);

  A.Free();
end;

function LoadResourceAsString(ResourceName: string): string;
var
  RS : TResourceStream;
  SS : TStringStream;
begin
  RS := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    RS.Position := 0;
    SS := TStringStream.Create();
    try
      SS.LoadFromStream(RS);
      Result := SS.DataString;
    finally
      SS.Free();
    end;
  finally
    RS.Free;
  end;
end;



end.

