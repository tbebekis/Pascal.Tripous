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
  , StdCtrls

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



  { App }
  App = class
  private class var
    FIsInitialized : Boolean;
    FMetaDatabases: TMetaDatabases;
    FConnectionInfo: TSqlConnectionInfo;
    FSqlStore : TSqlStore;

    tv: TTreeView;
    RootNode: TTreeNode;

    class function  GetTreeNodeMetaNode(Node: TTreeNode): TMetaNode;
    class function  GetIconIndex(MetaNodeType: TMetaNodeType): Integer;
    class procedure EnsureSqlConnection();
    class function  SelectSqlConnections(): IList<TSqlConInfoProxy>;
    class procedure AddDatabaseNodes();
    class procedure AddChildrenNodes(ParentNode: TTreeNode; ParentMetaNode: TMetaNode);
  public
    class procedure AppInitialize(TreeView: TTreeView);

    class function ConnectionInsert(ConInfoProxy: TSqlConInfoProxy): TMetaDatabase;

    class procedure AddDatabaseNode(MetaDatabase: TMetaDatabase);
    class procedure ReloadSelectedDatabase();

    class property IsInitialized: Boolean read FIsInitialized;
    class property MetaDatabases: TMetaDatabases read FMetaDatabases;
    class property SqlStore: TSqlStore read FSqlStore;
  end;

implementation

uses
  Tripous.Logs
  ;


{ App }
class procedure App.AppInitialize(TreeView: TTreeView);
begin
  if not IsInitialized then
  begin
    FIsInitialized := True;

    FMetaDatabases := TMetaDatabases.Create(False);
    EnsureSqlConnection();

    tv := TreeView;
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
    Result := TMetaNode(Node.Data);
  except
    Result := nil;
  end;
end;

class procedure App.ReloadSelectedDatabase();
var
  MetaDatabase: TMetaDatabase;
  MetaNode: TMetaNode;
begin
  if Assigned(tv.Selected) then
  begin
    Screen.Cursor := crHourGlass;
    try
      MetaNode := GetTreeNodeMetaNode(tv.Selected);
      if MetaNode is TMetaDatabase then
      begin
        MetaDatabase := MetaNode as TMetaDatabase;

        LogBox.AppendLine('Loading Database: ' + MetaDatabase.DisplayText + '. Please wait...');

        if MetaDatabase.Loaded then
        begin
          MetaDatabase.Clear();
          if tv.Selected.Count > 0 then
          begin
            tv.BeginUpdate();
            try
              tv.Selected.DeleteChildren();
            finally
              tv.EndUpdate();
            end;
          end;
        end;

        Application.ProcessMessages();
        MetaDatabase.Load();

        tv.BeginUpdate();
        try
          AddChildrenNodes(tv.Selected, MetaDatabase);
          LogBox.Append('DONE');
        finally
          tv.EndUpdate();
        end;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;


end.

