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
    class procedure EnsureSqlConnection();
    class function  SelectDatastores(): IList<TSqlConInfoProxy>;
    class procedure AddDatabaseNodes();
  public
    class procedure AppInitialize(TreeView: TTreeView);

    class function ConnectionInsert(ConInfoProxy: TSqlConInfoProxy): TMetaDatabase;

    class procedure AddDatabaseNode(MetaDatabase: TMetaDatabase);

    class property IsInitialized: Boolean read FIsInitialized;
    class property MetaDatabases: TMetaDatabases read FMetaDatabases;
    class property SqlStore: TSqlStore read FSqlStore;
  end;

implementation



{ App }
class procedure App.AppInitialize(TreeView: TTreeView);
begin
  if not IsInitialized then
  begin
    FIsInitialized := True;

    FMetaDatabases := TMetaDatabases.Create();
    EnsureSqlConnection();

    tv := TreeView;
    AddDatabaseNodes();
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

class function App.SelectDatastores(): IList<TSqlConInfoProxy>;
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

    List := SelectDatastores();
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

class procedure App.AddDatabaseNode(MetaDatabase: TMetaDatabase);
var
  Node: TTreeNode;
  Text: string;
begin
  Text := MetaDatabase.ConnectionInfo.Name + ' (' + MetaDatabase.ConnectionInfo.Provider + ')';
  Node := tv.Items.AddChild(RootNode, Text);
end;

end.

