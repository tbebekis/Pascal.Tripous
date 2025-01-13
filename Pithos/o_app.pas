unit o_App;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ComCtrls
  , Menus
  , ExtCtrls
  , DB
  //, StdCtrls

  ,Tripous
  ,Tripous.Data
  ,Tripous.MemTable
  ;

const
  SDatabaseFileName = 'Pithos.db3';
  STableSchema =
    'create table Datastores (                                                   '  +
    '    Id                  nvarchar(40)          not null primary key          '  +
    '   ,Name                nvarchar(96)          not null unique               '  +
    '   ,Provider            nvarchar(40)          not null                      '  +
    '   ,Server              nvarchar(40)          not null                      '  +
    '   ,Database            nvarchar(96)          not null                      '  +
    '   ,UserName            nvarchar(40)          null                          '  +
    '   ,Password            nvarchar(512)         null                          '  +
    '   ,Params              text                  null                          '  +
    ')                                                                           '  +
    ''
    ;


type

  { TSqlPageInfo }
  TSqlPageInfo = class
  private
    FInitialSql: string;
    FMetaDatabase: TMetaDatabase;
    FPage: TTabSheet;
  public
     constructor Create(MetaDatabase: TMetaDatabase; Page: TTabSheet; InitialSql: string);

     property MetaDatabase: TMetaDatabase read FMetaDatabase;
     property Page: TTabSheet read FPage;
     property InitialSql: string read FInitialSql;
  end;

  ISqlExecInfo = interface
  ['{9C9399D6-429D-4719-983D-C4978CC2C7CE}']
  {private}
    function GetIsSelect: Boolean;
    function GetSelectCounter: Integer;
    function GetSqlText: string;
    function GetStatementCounter: Integer;
    function  GetTable: TDataset;
    procedure SetTable(AValue: TDataset);
    function GetErrorText: string;
    procedure SetErrorText(AValue: string);
  {public}
    property SqlText: string read GetSqlText;
    property IsSelect: Boolean read GetIsSelect;
    property StatementCounter: Integer read GetStatementCounter;
    property SelectCounter: Integer read GetSelectCounter;
    property Table: TDataset read GetTable write SetTable;
    property ErrorText: string read GetErrorText write SetErrorText;
  end;

  { TSqlExecInfo }
  TSqlExecInfo = class(TInterfacedObject, ISqlExecInfo)
  private
    FSqlText: string;
    FIsSelect: Boolean;
    FStatementCounter: Integer;
    FSelectCounter: Integer;
    FTable: TDataset;
    FErrorText: string;

    function GetIsSelect: Boolean;
    function GetSelectCounter: Integer;
    function GetSqlText: string;
    function GetStatementCounter: Integer;
    function  GetTable: TDataset;

    procedure SetTable(AValue: TDataset);
    function GetErrorText: string;
    procedure SetErrorText(AValue: string);
  public
    constructor Create(SqlText: string; IsSelect: Boolean; StatementCounter: Integer; SelectCounter: Integer);

    property SqlText: string read GetSqlText;
    property IsSelect: Boolean read GetIsSelect;
    property StatementCounter: Integer read GetStatementCounter;
    property SelectCounter: Integer read GetSelectCounter;
    property Table: TDataset read GetTable write SetTable;
    property ErrorText: string read GetErrorText write SetErrorText;
  end;

  (*
  string sText = !string.IsNullOrWhiteSpace(Form.SyntaxEdit.SelectedText) ? Form.SyntaxEdit.SelectedText : Form.SyntaxEdit.Text;
  Form.History.Parse(sText);

  HistoryItem historyItem = Form.History.Current;
  if (historyItem != null)
  {
      foreach (StatementItem Statement in historyItem.SqlStatements)
      {
          Form.StatementCounter++;
          if (Statement.IsSelect)
          {
              Form.SelectCounter++;
          }

          SafeAsync SA = new SafeAsync();
          Dictionary<string, object> ArgList = new Dictionary<string, object>();
          ArgList["SafeAsync"] = SA;
          ArgList["IsSelect"] = Statement.IsSelect;
          ArgList["SqlText"] = Statement.SqlText;
          ArgList["StatementCounter"] = Form.StatementCounter;
          ArgList["SelectCounter"] = Form.SelectCounter;

          SA.Execute(ExecuteStatement, ArgList);
      }
  }
  *)

  { App }
  App = class
  private class var
    FIsInitialized : Boolean;
    FMetaDatabases: TMetaDatabases;
    FConnectionInfo: TSqlConnectionInfo;
    FSqlStore : TSqlStore;
    FSqlPageCounter: Integer;

    tv: TTreeView;
    Pager: TPageControl;
    RootNode: TTreeNode;

    class function  GetTreeNodeMetaNode(Node: TTreeNode = nil): TMetaNode;
    class function  GetTreeNodeMetaDatabase(Node: TTreeNode = nil): TMetaDatabase;

    class function  GetIconIndex(MetaNodeType: TMetaNodeType): Integer;
    class procedure EnsureSqlConnection();
    class function  SelectSqlConnections(): IList<TSqlConInfoProxy>;
    class procedure AddDatabaseNodes();
    class procedure AddChildrenNodes(ParentNode: TTreeNode; ParentMetaNode: TMetaNode);
  public
    class procedure AppInitialize(TreeView: TTreeView; Pager: TPageControl);

    class function  ConnectionInsert(ConInfoProxy: TSqlConInfoProxy): TMetaDatabase;

    class procedure AddSqlPage();

    class procedure AddDatabaseNode(MetaDatabase: TMetaDatabase);
    class procedure ReloadSelectedDatabase(Node: TTreeNode = nil);
    class procedure ReloadDatabase(MetaDatabase: TMetaDatabase);
    class function  NextSqlPageId(): Integer;

    class property IsInitialized: Boolean read FIsInitialized;
    class property MetaDatabases: TMetaDatabases read FMetaDatabases;
    class property SqlStore: TSqlStore read FSqlStore;
  end;

implementation

uses
  Tripous.Logs
  ,fr_ISqlFrame
  ;

{ TSqlPageInfo }

constructor TSqlPageInfo.Create(MetaDatabase: TMetaDatabase; Page: TTabSheet; InitialSql: string);
begin
  inherited Create();
  FMetaDatabase := MetaDatabase;
  FPage := Page;
  FInitialSql := InitialSql;
end;

{ TSqlExecInfo }
constructor TSqlExecInfo.Create(SqlText: string; IsSelect: Boolean;  StatementCounter: Integer; SelectCounter: Integer);
begin
  inherited Create();
  FSqlText := SqlText;
  FIsSelect := IsSelect;
  FStatementCounter := StatementCounter;
  FSelectCounter := SelectCounter;
end;
function TSqlExecInfo.GetIsSelect: Boolean;
begin
  Result := FIsSelect;
end;

function TSqlExecInfo.GetErrorText: string;
begin
  Result := FErrorText;
end;

function TSqlExecInfo.GetSelectCounter: Integer;
begin
   Result := FSelectCounter;
end;

function TSqlExecInfo.GetSqlText: string;
begin
   Result := FSqlText;
end;

function TSqlExecInfo.GetStatementCounter: Integer;
begin
  Result := FStatementCounter;
end;

function TSqlExecInfo.GetTable: TDataset;
begin
  Result := FTable;
end;

procedure TSqlExecInfo.SetErrorText(AValue: string);
begin
  FErrorText:= AValue;
end;

procedure TSqlExecInfo.SetTable(AValue: TDataset);
begin
  FTable := AValue;
end;




{ App }
class procedure App.AppInitialize(TreeView: TTreeView; Pager: TPageControl);
begin
  if not IsInitialized then
  begin
    FIsInitialized := True;

    FMetaDatabases := TMetaDatabases.Create(False);
    EnsureSqlConnection();

    tv := TreeView;
    App.Pager := Pager;
    AddDatabaseNodes();
  end;
end;



class function App.GetIconIndex(MetaNodeType: TMetaNodeType): Integer;
begin
  case MetaNodeType of
    ntDatabases : Result := 0;
    ntDatabase  : Result := 1;
    ntTables,
    ntFields,
    ntIndexes,
    ntPrimaryKeys,
    ntForeignKeys,
    ntConstraints,
    ntViews,
    ntTriggers,
    ntProcedures,
    ntSequences  : Result := 0;

    ntTable      : Result := 2;
    ntView       : Result := 3;
    ntField,
    ntIndex,
    ntPrimaryKey,
    ntForeignKey,
    ntConstraint,

    ntTrigger,
    ntProcedure,
    ntSequence   : Result := -1;
  else
    Result := -1;
  end;
end;

class procedure App.EnsureSqlConnection();
begin
  FConnectionInfo := TSqlConnectionInfo.Create();
  FConnectionInfo.Name := SDefaultConnectionName;
  FConnectionInfo.Provider := SqlProviders.SSqlite;
  FConnectionInfo.ConnectionString := Format('Server=localhost; Database=%s', [SDatabaseFileName]);

  FSqlStore := TSqlStore.Create(FConnectionInfo);

  if not FileExists(SDatabaseFileName) then
  begin
    Sys.CreateSqliteDatabase(SDatabaseFileName);

    FSqlStore.ExecSql(STableSchema);
  end;
end;

class function App.SelectSqlConnections(): IList<TSqlConInfoProxy>;
var
  SqlText: string;
  Table: TMemTable;
  SqlConInfoProxy: TSqlConInfoProxy;
begin
  Result := TGenObjectList<TSqlConInfoProxy>.Create(True, False);

  SqlText := 'select * from Datastores';
  Table := SqlStore.Select(SqlText);
  Table.First();
  while not Table.EOF do
  begin
    SqlConInfoProxy := TSqlConInfoProxy.CreateFromTable(Table);
    Result.Add(SqlConInfoProxy);
    Table.Next();
  end;
end;

class function App.ConnectionInsert(ConInfoProxy: TSqlConInfoProxy): TMetaDatabase;
begin
  FSqlStore.ExecSql(ConInfoProxy.GetInsertIntoSql(), [ConInfoProxy.ToDictionary()]);
  Result := MetaDatabases.Add(ConInfoProxy.CreateSqlConnectionInfo());
end;

class procedure App.AddSqlPage();
var
  MetaDatabase: TMetaDatabase;
  Page: TTabSheet;
  InitialSql: string;
  MetaNode : TMetaNode;
begin
  MetaDatabase := GetTreeNodeMetaDatabase(nil);
  if Assigned(MetaDatabase) then
  begin
    if not MetaDatabase.Loaded then
       ReloadDatabase(MetaDatabase)
    else begin
      MetaNode := GetTreeNodeMetaNode(tv.Selected);
      if Assigned(MetaNode) and ((MetaNode.NodeType = ntTable) or (MetaNode.NodeType = ntView)) then
        InitialSql := 'select * from ' + MetaNode.Name;
    end;

    Application.ProcessMessages();

    Page  := Pager.AddTabSheet();
    TISqlFrame.Create(Page, MetaDatabase, InitialSql);

    Pager.ActivePage := Page;
  end;

end;

class procedure App.AddDatabaseNodes();
var
  MetaDatabase: TMetaDatabase;

  List: IList<TSqlConInfoProxy>;
  Proxy: TSqlConInfoProxy;
  ConInfo: TSqlConnectionInfo;
begin
  tv.Items.BeginUpdate();
  try
    tv.Items.Clear();
    MetaDatabases.Clear();

    RootNode := tv.Items.Add(nil,'Databases');
    RootNode.Data := App.MetaDatabases;
    App.MetaDatabases.Tag := RootNode;
    RootNode.ImageIndex := GetIconIndex(App.MetaDatabases.NodeType);
    RootNode.SelectedIndex := RootNode.ImageIndex;

    List := SelectSqlConnections();
    for Proxy in List do
    begin
      ConInfo := Proxy.CreateSqlConnectionInfo();
      try
        MetaDatabase := MetaDatabases.Add(ConInfo);
        AddDatabaseNode(MetaDatabase);
      finally
        ConInfo.Free();
      end;
    end;
  finally
    tv.Items.EndUpdate();
  end;

end;

class procedure App.AddChildrenNodes(ParentNode: TTreeNode; ParentMetaNode: TMetaNode);
var
  ChildrenMetaNodes: TMetaNodeArray;
  MetaNode: TMetaNode;
  Node: TTreeNode;
begin
  ChildrenMetaNodes :=  ParentMetaNode.Nodes;
  if Length(ChildrenMetaNodes) > 0 then
  begin
    for MetaNode in ChildrenMetaNodes do
    begin
      Node := tv.Items.AddChild(ParentNode, MetaNode.DisplayText);
      Node.Data := MetaNode;
      MetaNode.Tag := Node;
      Node.ImageIndex := GetIconIndex(MetaNode.NodeType);
      Node.SelectedIndex := Node.ImageIndex;
      AddChildrenNodes(Node, MetaNode);
    end;
  end;
end;

class procedure App.AddDatabaseNode(MetaDatabase: TMetaDatabase);
var
  Node: TTreeNode;
begin
  tv.Items.BeginUpdate();
  try
    Node := tv.Items.AddChild(RootNode, MetaDatabase.DisplayText);
    Node.Data := MetaDatabase;
    MetaDatabase.Tag := Node;
    Node.ImageIndex := GetIconIndex(MetaDatabase.NodeType);
    Node.SelectedIndex := Node.ImageIndex;
    //AddChildrenNodes(Node, MetaDatabase);
  finally
    tv.Items.EndUpdate();
  end;
end;

class function  App.GetTreeNodeMetaNode(Node: TTreeNode): TMetaNode;
begin
  try
    if not Assigned(Node) then
      Node := tv.Selected;

    if Assigned(Node) then
       Result := TMetaNode(Node.Data);
  except
    Result := nil;
  end;
end;

class function App.GetTreeNodeMetaDatabase(Node: TTreeNode): TMetaDatabase;
var
  MetaNode: TMetaNode;
begin
  Result := nil;
  if not Assigned(Node) then
     Node := tv.Selected;

  if Assigned(Node) then
  begin
    MetaNode := GetTreeNodeMetaNode(Node);
    if Assigned(MetaNode) then
       Result := MetaNode.Database;
  end;
end;

class procedure App.ReloadSelectedDatabase(Node: TTreeNode);
var
  MetaDatabase: TMetaDatabase;
begin
  if not Assigned(Node) then
     Node := tv.Selected;

  if Assigned(Node) then
  begin
     MetaDatabase := GetTreeNodeMetaDatabase(Node);
     ReloadDatabase(MetaDatabase);
  end;
end;

class procedure App.ReloadDatabase(MetaDatabase: TMetaDatabase);
var
  Node: TTreeNode;
begin
  if Assigned(MetaDatabase) then
  begin
    Node := MetaDatabase.Tag as TTreeNode;

    if Assigned(Node) then
    begin
      Screen.Cursor := crHourGlass;
      try
        LogBox.AppendLine('Loading Database: ' + MetaDatabase.DisplayText + '. Please wait...');

        if MetaDatabase.Loaded then
        begin
          MetaDatabase.Clear();
          if Node.Count > 0 then
          begin
            tv.BeginUpdate();
            try
              Node.DeleteChildren();
            finally
              tv.EndUpdate();
            end;
          end;
        end;

        Application.ProcessMessages();
        MetaDatabase.Load();

        tv.BeginUpdate();
        try
          AddChildrenNodes(Node, MetaDatabase);
          Node.Expand(False);
          LogBox.Append('DONE');
        finally
          tv.EndUpdate();
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;

end;

class function App.NextSqlPageId(): Integer;
begin
  Inc(FSqlPageCounter);
  Result := FSqlPageCounter;
end;


end.

