unit o_App;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils

  ,Tripous
  ,Tripous.Data
  ,Tripous.MemTable
  ;

const
  SDatabaseFileName = 'Pithos.db3';
  STableSchema =
    'create table Datastores (                                            '  +
    '    Id                  nvarchar(40)          not null primary key   '  +
    '   ,Name                nvarchar(96)          not null unique        '  +
    '   ,Provider            nvarchar(40)          not null               '  +
    '   ,ConStr              text                  not null               '  +
    ')                                                                    '  +
    ''
  ;

type

  { TConInfoProxy }
  TConInfoProxy = class
  private
    FConnectionString: string;
    FId: string;
    FName: string;
    FProvider: string;
    FTag: TObject;
  public
    constructor Create();

    property Id: string read FId write FId;
    { The main connection must be named 'Default', e.g. ConInfo.Name := 'Default'; }
    property Name: string read FName write FName;
    property Provider: string read FProvider write FProvider;
    { e.g. Server=localhost; Database=MyDb; User=admin; Psw=p@s$W0rD }
    property ConnectionString: string read FConnectionString write FConnectionString;
    property Tag: TObject read FTag write FTag;
  end;

  { App }
  App = class
  private class var
    FIsInitialized : Boolean;
    FConnectionInfo: TSqlConnectionInfo;
    FSqlStore : TSqlStore;
    class procedure EnsureSqlConnection();
  public
    class procedure AppInitialize();

    class property IsInitialized                  : Boolean read FIsInitialized;
    class property SqlStore: TSqlStore read FSqlStore;
  end;

implementation

{ TConInfoProxy }

constructor TConInfoProxy.Create();
begin
  inherited Create;
  FConnectionString := 'Server=?; Database=?; UserId=?; Password=?';
  FProvider := SqlProviders.SSqlite;
end;

{ App }
class procedure App.AppInitialize();

begin
  if not IsInitialized then
  begin
    FIsInitialized := True;

    EnsureSqlConnection();

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

end.

