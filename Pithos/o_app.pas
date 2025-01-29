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
  ,Tripous.Ui
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

  { App }
  App = class
  private class var
    FIsInitialized : Boolean;
    FConInfoProxyList: IList<TSqlConInfoProxy>;
    FMetaDatabases: TMetaDatabases;
    FConnectionInfo: TSqlConnectionInfo;
    FSqlStore : TSqlStore;

    tv: TTreeView;
    Pager: TPageControl;
    RootNode: TTreeNode;

    class function  GetTreeNodeMetaNode(Node: TTreeNode = nil): TMetaNode;
    class function  GetTreeNodeMetaDatabase(Node: TTreeNode = nil): TMetaDatabase;

    class function  FindConInfoProxy(MetaDatabase: TMetaDatabase): TSqlConInfoProxy;

    class function  GetIconIndex(MetaNodeType: TMetaNodeType): Integer;
    class procedure EnsureOwnDatabaseConnection();
    class procedure SelectDatabases();
    class procedure AddDatabaseNodes();
    class procedure AddChildrenNodes(ParentNode: TTreeNode; ParentMetaNode: TMetaNode);
  public
    class procedure AppInitialize(TreeView: TTreeView; Pager: TPageControl);

    class procedure InsertDatabase();
    class procedure EditDatabase();
    class procedure RemoveDatabase();
    class procedure CreateDatabase();

    class procedure AddISqlPage(InitialSql: string = '');
    class procedure AddFieldListPage();
    class procedure AddMetadataPage();

    class procedure AddDatabaseNode(MetaDatabase: TMetaDatabase);
    class procedure ReloadSelectedDatabase(Node: TTreeNode = nil);
    class procedure ReloadDatabase(MetaDatabase: TMetaDatabase);

    class procedure SelectTableOrView();

    class property IsInitialized: Boolean read FIsInitialized;
    class property MetaDatabases: TMetaDatabases read FMetaDatabases;
    class property SqlStore: TSqlStore read FSqlStore;
  end;

implementation

uses
   Tripous.Logs
  ,f_ConnectionEditDialog
  ,f_ISqlForm
  ,f_SqlEditorForm
  ;

{ TSqlPageInfo }

constructor TSqlPageInfo.Create(MetaDatabase: TMetaDatabase; Page: TTabSheet; InitialSql: string);
begin
  inherited Create();
  FMetaDatabase := MetaDatabase;
  FPage := Page;
  FInitialSql := InitialSql;
end;





{ App }
class procedure App.AppInitialize(TreeView: TTreeView; Pager: TPageControl);
begin
  if not IsInitialized then
  begin
    FIsInitialized := True;

    FConInfoProxyList := TGenObjectList<TSqlConInfoProxy>.Create(True, False);

    FMetaDatabases := TMetaDatabases.Create(False);
    EnsureOwnDatabaseConnection();

    tv := TreeView;
    App.Pager := Pager;
    AddDatabaseNodes();
  end;
end;

class procedure App.EnsureOwnDatabaseConnection();
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

class procedure App.SelectDatabases();
var
  SqlText: string;
  Table: TMemTable;
  SqlConInfoProxy: TSqlConInfoProxy;
begin
  FConInfoProxyList.Clear();

  SqlText := 'select * from Datastores';
  Table := SqlStore.Select(SqlText);
  Table.First();
  while not Table.EOF do
  begin
    SqlConInfoProxy := TSqlConInfoProxy.CreateFromTable(Table);
    FConInfoProxyList.Add(SqlConInfoProxy);
    Table.Next();
  end;
end;

class procedure App.AddDatabaseNodes();
var
  MetaDatabase: TMetaDatabase;

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

    SelectDatabases();
    for Proxy in FConInfoProxyList do
    begin
      ConInfo := Proxy.CreateSqlConnectionInfo();
      try
        MetaDatabase := MetaDatabases.Add(ConInfo);
        Proxy.Tag := MetaDatabase;
        AddDatabaseNode(MetaDatabase);
      finally
        ConInfo.Free();
      end;
    end;
  finally
    tv.Items.EndUpdate();
  end;

  if Assigned(RootNode) and (RootNode.Count > 0) then
     RootNode.Expand(False);

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


class procedure App.InsertDatabase();
var
  ConInfoProxy: TSqlConInfoProxy;
  MetaDatabase: TMetaDatabase;
begin
  ConInfoProxy := TSqlConInfoProxy.Create();
  if TConnectionEditDialog.ShowDialog(ConInfoProxy, cdmInsert) then
  begin
     FSqlStore.ExecSql(ConInfoProxy.GetInsertIntoSql(), [ConInfoProxy.ToDictionary()]);
     MetaDatabase := MetaDatabases.Add(ConInfoProxy.CreateSqlConnectionInfo());
     ConInfoProxy.Tag := MetaDatabase;
     FConInfoProxyList.Add(ConInfoProxy);
     App.AddDatabaseNode(MetaDatabase);
  end;

end;

class procedure App.EditDatabase();
var
  MetaDatabase: TMetaDatabase;
  ConInfoProxy: TSqlConInfoProxy;
  Node: TTreeNode;
begin
  MetaDatabase := GetTreeNodeMetaDatabase();
  if Assigned(MetaDatabase) then
  begin
    ConInfoProxy := FindConInfoProxy(MetaDatabase);
    if Assigned(ConInfoProxy) then
    begin
      if TConnectionEditDialog.ShowDialog(ConInfoProxy, cdmEdit) then
      begin
        FSqlStore.ExecSql(ConInfoProxy.GetUpdateSql(), [ConInfoProxy.ToDictionary()]);
        ReloadDatabase(MetaDatabase);
        Node := TTreeNode(MetaDatabase.Tag);

        MetaDatabase.ConnectionInfo.Name := ConInfoProxy.Name;
        Node.Text := MetaDatabase.DisplayText;
        Node.Update();
      end;
    end;
  end;
end;

class procedure App.RemoveDatabase();
var
  MetaDatabase: TMetaDatabase;
  ConInfoProxy: TSqlConInfoProxy;
  Node: TTreeNode;
  Message: string;
begin
  MetaDatabase := GetTreeNodeMetaDatabase();
  if Assigned(MetaDatabase) then
  begin
    ConInfoProxy := FindConInfoProxy(MetaDatabase);
    if Assigned(ConInfoProxy) then
    begin
      Message :=
      'This action will remove the database connection' +  sLineBreak +  sLineBreak +
      '    %s ' +  sLineBreak +  sLineBreak +
      'from this application.' +  sLineBreak +  sLineBreak +
      'This action will NOT remove the database.' +  sLineBreak +  sLineBreak +
      'Do you want to continue?'
      ;

      Message := Format(Message, [MetaDatabase.DisplayText]);

      if Ui.YesNoBox(Message) then
      begin
        Node := TTreeNode(MetaDatabase.Tag);
        tv.Items.Delete(Node);
        FSqlStore.ExecSql(ConInfoProxy.GetDeleteSql(), [ConInfoProxy.ToDictionary()]);
        FConInfoProxyList.Remove(ConInfoProxy);
        FMetaDatabases.Remove(MetaDatabase);
      end;
    end;

  end;

end;

class procedure App.CreateDatabase();
begin

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

class procedure App.ReloadSelectedDatabase(Node: TTreeNode);
var
  MetaDatabase: TMetaDatabase;
  MetaNode: TMetaNode;
begin
  MetaNode := GetTreeNodeMetaNode(Node);
  if Assigned(MetaNode) and (MetaNode.NodeType = ntDatabase) then
  begin
    MetaDatabase := MetaNode as TMetaDatabase;
    ReloadDatabase(MetaDatabase);
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

class procedure App.AddISqlPage(InitialSql: string);
var
  MetaDatabase: TMetaDatabase;
begin
  MetaDatabase := GetTreeNodeMetaDatabase(nil);
  if Assigned(MetaDatabase) then
  begin
    if not MetaDatabase.Loaded then
       ReloadDatabase(MetaDatabase);

    Application.ProcessMessages();

    TISqlForm.CreateAsChild(Pager, MetaDatabase, InitialSql);
  end;

end;

class procedure App.AddFieldListPage();
var
  MetaNode: TMetaNode;
begin
  MetaNode := GetTreeNodeMetaNode();
  if Assigned(MetaNode) then
     TSqlEditorForm.CreateAsChildForFieldList(Pager, MetaNode);
end;

class procedure App.AddMetadataPage();
var
  MetaNode: TMetaNode;
begin
  MetaNode := GetTreeNodeMetaNode();
  if Assigned(MetaNode) then
     TSqlEditorForm.CreateAsChildForMetadata(Pager, MetaNode);
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

class procedure App.SelectTableOrView();
var
  MetaNode: TMetaNode;
  SqlText: string;
begin
  MetaNode := GetTreeNodeMetaNode();
  if Assigned(MetaNode) then
     if (MetaNode.NodeType = ntTable) or (MetaNode.NodeType = ntView) then
     begin
       SqlText := MetaNode.Database.SqlStore.Provider.SelectTop(MetaNode.Name);
       AddISqlPage(SqlText);
     end;
end;

class function App.FindConInfoProxy(MetaDatabase: TMetaDatabase): TSqlConInfoProxy;
var
  Item: TSqlConInfoProxy;
begin
  for Item in FConInfoProxyList do
    if Item.Tag = MetaDatabase then
       Exit(Item);

  Result := nil;
end;


end.

