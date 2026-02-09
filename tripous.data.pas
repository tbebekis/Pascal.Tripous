unit Tripous.Data;

{$MODE DELPHI}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
   Classes
  ,SysUtils
  ,DateUtils
  ,LazSysUtils
  ,Variants
  ,DB
  ,SQLDBLib
  ,sqldb

  ,bufdataset

  ,IBConnection
  ,mssqlconn
  ,mysql80conn
  ,pqconnection
  ,sqlite3conn
  ,oracleconnection
  ,odbcconn

  ,Types

  ,Tripous
  ,Tripous.MemTable

  ,Dialogs
  ;

const
  SDefaultConnectionName = 'Default';

type
  TMetaNodeType = (
    ntNone,
    ntDatabases,
    ntDatabase,
    ntTables,
    ntTable,
    ntFields,
    ntField,
    ntIndexes,
    ntIndex,
    ntPrimaryKeys,
    ntPrimaryKey,
    ntForeignKeys,
    ntForeignKey,
    ntConstraints,
    ntConstraint,
    ntViews,
    ntView,
    ntTriggers,
    ntTrigger,
    ntProcedures,
    ntProcedure,
    ntSequences,
    ntSequence
  );

  TConstraintType = (
    ctUnknown = 0,
    ctPrimaryKey = 1,
    ctForeignKey = 2,
    ctUnique = 3,
    ctCheck = 4,
    ctNotNull = 5
  );



  TSqlProviderType = (
    ptNone,
    ptFirebird,
    ptSqlite,
    ptMsSql,
    ptMySql,
    ptPostgreSql,
    ptOracle
  );

  TSqlProviderTypeArray = array of TSqlProviderType;


  TSetOfFieldType    = set of TFieldType;



  { forward }
  TSqlProvider = class;
  TSqlConnectionInfo = class;
  TSqlConnectionInfoArray = array of TSqlConnectionInfo;
  TMetaNode = class;
  TMetaNodeArray = array of TMetaNode;
  TMetaDatabase = class;
  TSqlStore = class;



  { TSQLConnectorEx }
  TSQLConnectorEx = class(TSQLConnector)
  protected
    procedure CreateProxy; override;
  public
    property Proxy;
  end;

  { TMetaNode }
  TMetaNode = class
  protected
    FName: string;
    FSchemaName: string;
    FNodeType: TMetaNodeType;
    FDatabase: TMetaDatabase;
    FParentNode: TMetaNode;
    FTag: TObject;
    FIsContainer: Boolean;

    function GetDisplayText: string; virtual;
    function GetNodes: TMetaNodeArray;  virtual;

    procedure DoClear(); virtual;

    function  ListToArray(List: TGenObjectList<TMetaNode>): TMetaNodeArray; virtual;
    function  UnQuote(const S: string): string;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string; ANodeType: TMetaNodeType);

    function GetDefinition(): string; virtual;
    function GetParentTableOrView(): TMetaNode; virtual;

    property Database: TMetaDatabase read FDatabase;
    property NodeType: TMetaNodeType read FNodeType;
    property IsContainer: Boolean read FIsContainer;
    property SchemaName: string read FSchemaName;
    property Name: string read FName;
    property DisplayText: string read GetDisplayText;

    property Nodes: TMetaNodeArray read GetNodes;

    property ParentNode: TMetaNode read FParentNode;
    property Tag: TObject read FTag write FTag;
  end;

  { TMetaField }
  TMetaField = class(TMetaNode)
  private
    FDataSubType: string;
    FDataType: string;
    FDefaultValue: string;
    FExpression: string;
    FIsNullable: Boolean;
    FOrdinalPosition: Integer;
    FPrecision: Integer;
    FScale: Integer;
    FSizeInBytes: Integer;
    FSizeInChars: Integer;
  protected
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;
    function GetText(NameOnly: Boolean): string;

    property DataType: string read FDataType;
    property DataSubType: string read FDataSubType;
    property IsNullable: Boolean read FIsNullable;
    property SizeInChars: Integer read FSizeInChars;
    property SizeInBytes: Integer read FSizeInBytes;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
    property DefaultValue: string read FDefaultValue;
    property Expression: string read FExpression;    // when is a calculated field
    property OrdinalPosition: Integer read FOrdinalPosition;
  end;

  { TMetaFields }
  TMetaFields = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaField>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaField;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function GetFieldListText(NamesOnly: Boolean): string;

    function Find(const AName: string; const ASchemaName: string): TMetaField;

    property List: TGenObjectList<TMetaField> read FList;
  end;

  { TMetaIndex }
  TMetaIndex = class(TMetaNode)
  private
    FFields: string;
    FIndexType: string;
    FIsUnique: Boolean;
  protected
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;

    property IndexType: string read FIndexType;
    property IsUnique: Boolean read FIsUnique;
    property Fields: string read FFields;
  end;

  { TMetaIndexes }
  TMetaIndexes = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaIndex>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaIndex;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaIndex;

    property List: TGenObjectList<TMetaIndex> read FList;
  end;

  { TMetaTrigger }
  TMetaTrigger = class(TMetaNode)
  private
    FDefinition: string;
    FTableName: string;
    FTriggerType: string;
  protected
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;

    property TriggerType: string read FTriggerType;
    property TableName: string read FTableName;
    property Definition: string read FDefinition;
  end;

  { TMetaTriggers }
  TMetaTriggers = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaTrigger>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaTrigger;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaTrigger;

    property List: TGenObjectList<TMetaTrigger> read FList;
  end;

  { TMetaConstraint }
  TMetaConstraint = class(TMetaNode)
  private
    FConstraintType: TConstraintType;
    FConstraintTypeText: string;
    FFields: string;
  protected
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;

    property ConstraintTypeText: string read FConstraintTypeText;
    property ConstraintType: TConstraintType read FConstraintType;
    property Fields: string read FFields;
  end;

  { TMetaConstraints }
  TMetaConstraints = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaConstraint>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaConstraint;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaConstraint;

    property List: TGenObjectList<TMetaConstraint> read FList;
  end;

  { TMetaPrimaryKey }
  TMetaPrimaryKey = class(TMetaConstraint)
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;

  end;

  { TMetaPrimaryKeys }
  TMetaPrimaryKeys = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaPrimaryKey>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaPrimaryKey;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaPrimaryKey;

    property List: TGenObjectList<TMetaPrimaryKey> read FList;
  end;

  { TMetaForeignKey }
  TMetaForeignKey = class(TMetaConstraint)
  private
    FForeignFields: string;
    FForeignTable: string;
    FUpdateRule: string;
    FDeleteRule: string;
  protected
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);

    function GetDefinition(): string; override;

    property ForeignTable: string read FForeignTable;
    property ForeignFields: string read FForeignFields;
    property UpdateRule: string read FUpdateRule;
    property DeleteRule: string read FDeleteRule;
  end;

  { TMetaForeignKeys }
  TMetaForeignKeys = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaForeignKey>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(AName: string; const ASchemaName: string): TMetaForeignKey;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaForeignKey;

    property List: TGenObjectList<TMetaForeignKey> read FList;
  end;


  { TMetaTable }
  TMetaTable = class(TMetaNode)
  private
    FConstraints: TMetaConstraints;
    FFields: TMetaFields;
    FForeignKeys: TMetaForeignKeys;
    FIndexes: TMetaIndexes;
    FPrimaryKeys: TMetaPrimaryKeys;
    FTriggers: TMetaTriggers;
  protected
    procedure DoClear(); override;
    function GetNodes: TMetaNodeArray;  override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
    destructor Destroy(); override;

    function GetDefinition(): string; override;

    property Fields: TMetaFields read FFields;
    property PrimaryKeys: TMetaPrimaryKeys read FPrimaryKeys;
    property ForeignKeys: TMetaForeignKeys read FForeignKeys;
    property Constraints: TMetaConstraints read FConstraints;
    property Indexes: TMetaIndexes read FIndexes;
    property Triggers: TMetaTriggers read FTriggers;
  end;

  { TMetaTables }
  TMetaTables = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaTable>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(AName: string; const ASchemaName: string): TMetaTable;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaTable;

    property List: TGenObjectList<TMetaTable> read FList;
  end;

  { TMetaView }
  TMetaView = class(TMetaNode)
  private
    FDefinition: string;
    FFields: TMetaFields;
  protected
    procedure DoClear(); override;
    function GetNodes: TMetaNodeArray;  override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
    destructor Destroy(); override;

    function GetDefinition(): string; override;

    property Fields: TMetaFields read FFields;
    property Definition: string read FDefinition;
  end;

  { TMetaViews }
  TMetaViews = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaView>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(AName: string; const ASchemaName: string): TMetaView;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaView;

    property List: TGenObjectList<TMetaView> read FList;
  end;

  { TMetaProcedure }
  TMetaProcedure = class(TMetaNode)
  private
    FDefinition: string;
    FProcedureType: string;
  protected
    procedure DoClear(); override;
    function  GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
    destructor Destroy(); override;

    function GetDefinition(): string; override;

    property ProcedureType: string read FProcedureType;
    property Definition: string read FDefinition;
  end;

  { TMetaProcedures }

  TMetaProcedures = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaProcedure>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(AName: string; const ASchemaName: string): TMetaProcedure;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaProcedure;

    property List: TGenObjectList<TMetaProcedure> read FList;
  end;


  { TMetaSequence }
  TMetaSequence = class(TMetaNode)
  private
    FCurrentValue: Integer;
    FInitialValue: Integer;
    FIncrementBy: Integer;
  protected
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
    destructor Destroy(); override;

    function GetDefinition(): string; override;

    property CurrentValue: Integer read FCurrentValue;
    property InitialValue: Integer read FInitialValue;
    property IncrementBy: Integer read FIncrementBy;
  end;

  { TMetaSequences }
  TMetaSequences = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaSequence>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(AName: string; const ASchemaName: string): TMetaSequence;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string; const ASchemaName: string): TMetaSequence;

    property List: TGenObjectList<TMetaSequence> read FList;
  end;

  { TMetaDatabase }

  TMetaDatabase = class(TMetaNode)
  private
    FConnectionInfo: TSqlConnectionInfo;
    FProcedures: TMetaProcedures;
    FSequences: TMetaSequences;
    FTables: TMetaTables;
    FViews: TMetaViews;
    FSqlStore: TSqlStore;
    FLoaded: Boolean;
    FLoading: Boolean;
    FSqlProviderType: TSqlProviderType;

    procedure LoadField(tblSql: TMemTable; MetaField: TMetaField);

    procedure LoadTables();
    procedure LoadTableFields();
    procedure LoadViews();
    procedure LoadViewFields();
    procedure LoadIndexes();
    procedure LoadTriggers();
    procedure LoadProcedures();
    procedure LoadConstraints();
    procedure LoadSequences();

    function  ProcessReplacementsInMetadataSql(SqlText: string): string;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function GetDisplayText: string; override;
  public
    constructor Create(AParentNode: TMetaNode; ConnectionInfo: TSqlConnectionInfo);
    destructor Destroy(); override;

    procedure Clear();
    procedure Load();

    property ConnectionInfo: TSqlConnectionInfo read FConnectionInfo;

    property Tables: TMetaTables read FTables;
    property Views: TMetaViews read FViews;
    property Procedures: TMetaProcedures read FProcedures;
    property Sequences: TMetaSequences read FSequences;

    property Loading: Boolean read FLoading;
    property Loaded: Boolean read FLoaded;

    property SqlStore: TSqlStore read FSqlStore;
    property SqlProviderType: TSqlProviderType read FSqlProviderType;
  end;

  { TMetaDatabases }

  TMetaDatabases = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaDatabase>;
    FIsAutoLoad: Boolean;
  protected
    function GetNodes: TMetaNodeArray;  override;
  public
    constructor Create(AutoLoad: Boolean);
    destructor Destroy(); override;

    procedure Clear();
    procedure ReLoad();

    function  Add(ConnectionInfo: TSqlConnectionInfo): TMetaDatabase;
    procedure AddRange(ConnectionInfoList: TSqlConnectionInfoArray);
    procedure Remove(ADatabase: TMetaDatabase);

    property List: TGenObjectList<TMetaDatabase> read FList;
    property IsAutoLoad: Boolean read FIsAutoLoad;
  end;


  { TSqlConnectionInfo }
  TSqlConnectionInfo = class(TCollectionItem)
  private class var
    FCounter: LargeUint;
  private
    FAutoCreateGenerators: Boolean;
    FCharSet: string;
    FConnectionString: string;
    //FConnectorType: string;
    FDatabase: string;
    FServer: string;
    FId: LargeUint;
    FName: string;
    FPassword: string;
    FProvider: string;
    FUserName: string;
    FParams: TStrings;
    function GetConnectorType: string;
    procedure SetConnectionString(AValue: string);
  public
    constructor Create(ACollection: TCollection = nil); override;
    destructor Destroy(); override;

    procedure Assign(Source: TPersistent); override;
    function Clone(): TSqlConnectionInfo;
    procedure Clear();

    function  GetSqlProvider(): TSqlProvider;
    procedure SetupConnection(SqlConnector: TSQLConnectorEx);
    function  CanConnect(ThrowIfNot: Boolean = False): Boolean;

    property Id : LargeUint read FId;

    property ConnectorType: string read GetConnectorType;
    property Database: string read FDatabase;
    property Server: string read FServer;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property CharSet: string read FCharSet;
    property Params: TStrings read FParams;
  published
    { The main connection must be named 'Default',
      e.g. ConInfo.Name := 'Default'; }
    property Name: string read FName write FName;
    { Firebird, MsSql, MySql, Sqlite, PostgreSql, Oracle
      e.g. ConInfo.Provider := SqlProviders.ProviderTypeToString(ptFirebird) }
    property Provider: string read FProvider write FProvider;
    { e.g. Server=localhost; Database=C:\Path\To\DB.FDB; User=SYSDBA; Psw=masterkey; Charset=UTF8
      e.g. Server=localhost; Database=AdventureWorksLT2012; User=sa; Psw=p@$W03d; TrustServerCertificate=true }
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property AutoCreateGenerators: Boolean read FAutoCreateGenerators write FAutoCreateGenerators;
  end;

  { TSqlConnectionInfoListEnumerator }
  TSqlConnectionInfoListEnumerator = class(TEnumeratorBase)
  private
    function GetCurrent: TSqlConnectionInfo;
  public
    property Current: TSqlConnectionInfo read GetCurrent;
  end;

  { TSqlConnectionInfoList }
  TSqlConnectionInfoList = class(TPersistent)
  private
    fSqlConnections: TCollection;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSqlConnectionInfo;
  public
    const DefaultFilePath = 'SqlConnections.json';

    constructor Create();
    destructor Destroy(); override;

    procedure Assign(Source: TPersistent); override;
    function  Clone(): TSqlConnectionInfoList;
    procedure Clear();

    procedure Load(FilePath: string = DefaultFilePath);
    procedure Save(FilePath: string = DefaultFilePath);

    procedure Remove(Item: TSqlConnectionInfo);
    procedure Add(Item: TSqlConnectionInfo);

    function  IndexOf(Item: TSqlConnectionInfo): Integer;
    function  Contains(const Name: string): Boolean;
    function  Find(const Name: string): TSqlConnectionInfo;

    function GetEnumerator(): TSqlConnectionInfoListEnumerator;

    property Items[Index: Integer]: TSqlConnectionInfo read GetItem ; default;
    property Count: Integer read GetCount;
  published
    property SqlConnections: TCollection read fSqlConnections write fSqlConnections;
  end;

  { TSqlConInfoProxy }
  TSqlConInfoProxy = class(TPersistent)
  private
    FDatabase: string;
    FId: string;
    FName: string;
    FParams: TStrings;
    FPassword: string;
    FProvider: string;
    FServer: string;
    FTag: TObject;
    FUserName: string;
  public
    constructor Create();
    constructor CreateFromTable(Table: TDataset);
    destructor Destroy(); override;

    function  CreateSqlParamList(): TStrings;
    procedure ToSqlParams(SqlParams : TParams); overload;
    procedure ToSqlParams(Q: TSQLQuery); overload;
    function  ToDictionary(): IDictionary<string, Variant>;
    function  GetInsertIntoSql(): string;
    function  GetUpdateSql(): string;
    function  GetDeleteSql(): string;

    procedure FromConnectionInfo(SqlConInfo: TSqlConnectionInfo);
    procedure ToConnectionInfo(SqlConInfo: TSqlConnectionInfo);

    function  CreateSqlConnectionInfo(): TSqlConnectionInfo;

    function  ToConnectionString(): string;
    procedure LoadFromTable(Table: TDataset);

    property Tag: TObject read FTag write FTag;
  published
    property Id: string read FId write FId;

    property Name: string read FName write FName;
    property Provider: string read FProvider write FProvider;

    property Server: string read FServer write FServer;
    property Database: string read FDatabase write FDatabase;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Params: TStrings read FParams write FParams;
  end;

  { SqlProviders }

  SqlProviders = class
  private class var
    Providers : IList<TSqlProvider>;
  public
    const SFirebird = 'Firebird';
    const SSqlite = 'Sqlite';
    const SMsSql = 'MsSql';
    const SMySql = 'MySql';
    const SPostgreSql = 'PostgreSql';
    const SOracle = 'Oracle';

    const ProviderNames: TStringArray = [SFirebird, SSqlite, SMsSql, SMySql, SPostgreSql, SOracle];
    const ProviderTypes: TSqlProviderTypeArray = [ptFirebird, ptSqlite, ptMsSql, ptMySql, ptPostgreSql, ptOracle];

    class constructor Create();
    class destructor Destroy();

    class function ProviderTypeToString(ProviderType: TSqlProviderType): string;
    class function StringToProviderType(ProviderType: string): TSqlProviderType;

    class function FindSqlProvider(const ProviderName: string): TSqlProvider;
  end;

  { TSqlProvider }
  TSqlProvider = class
  private
    FName: string;
    FProviderType: TSqlProviderType;
  protected
    FLibraryLoader : TSQLDBLibraryLoader;

    FTablesSql: string;
    FViewsSql: string;
    FTableFieldsSql: string;
    FViewFieldsSql: string;
    FIndexesSql: string;
    FTriggersSql: string;
    FConstraintsSql: string;
    FProceduresSql: string;
    FSequencesSql: string;

    function GetConnectorType: string; virtual;

    function GetTablesSql(): string; virtual;
    function GetViewsSql(): string; virtual;
    function GetTableFieldsSql(): string; virtual;
    function GetViewFieldsSql(): string; virtual;
    function GetIndexesSql(): string; virtual;
    function GetTriggersSql(): string; virtual;
    function GetConstraintsSql(): string; virtual;
    function GetProceduresSql(): string; virtual;
    function GetSequencesSql(): string; virtual;
  public
    constructor Create(ProviderType: TSqlProviderType);
    destructor Destroy(); override;

    class function ProviderTypeToProviderName(ProviderType: TSqlProviderType): string;
    class function ProviderTypeToConnectorType(ProviderType: TSqlProviderType): string;

    class function ProviderNameToProviderType(ProviderName: string): TSqlProviderType;
    class function ProviderNameToConnectorType(ProviderName: string): string;

    procedure ActivateLibraryLoader(); virtual;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; virtual; abstract;

    function CanConnect(ConnectionString: string; ThrowIfNot: Boolean = False): Boolean; overload;
    function CanConnect(ConInfo: TSqlConnectionInfo; ThrowIfNot: Boolean = False): Boolean; overload;

    { Returns the current date and time of the database serve }
    function GetServerDateTime(Store: TSqlStore): TDateTime; virtual;
    { Quotes and formats a date value as a string, properly for use with an Sql statement }
    function  QSDate(Value: TDateTime): string; virtual;
    { Quotes and formats a date-time value as a string, properly for use with an Sql statement }
    function  QSDateTime(Value: TDateTime): string; virtual;


    property Name: string read FName;
    property ProviderType: TSqlProviderType read FProviderType;

    // metadata SELECTs
    property TablesSql:      string read GetTablesSql;
    property TableFieldsSql: string read GetTableFieldsSql;
    property ViewsSql:       string read GetViewsSql;
    property ViewFieldsSql:  string read GetViewFieldsSql;
    property IndexesSql:     string read GetIndexesSql;
    property TriggersSql:    string read GetTriggersSql;
    property ConstraintsSql: string read GetConstraintsSql;
    property ProceduresSql:  string read GetProceduresSql;
    property SequencesSql:   string read GetSequencesSql;

    property ConnectorType:  string read GetConnectorType;
  end;

  { TSqlProviderFirebird - 'User = SYSDBA; Psw = password; Database = C:\Program Files\Firebird\Firebird_5_0\examples\empbuild\EMPLOYEE.FDB';  }
  TSqlProviderFirebird = class(TSqlProvider)
  public
    constructor Create();
    destructor Destroy(); override;

    { Returns the current date and time of the database serve }
    function GetServerDateTime(Store: TSqlStore): TDateTime; override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlProviderSqlite }

  TSqlProviderSqlite = class(TSqlProvider)
  public
    constructor Create();
    destructor Destroy(); override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlProviderMsSql

  Download the dblib_current.zip
  from https://downloads.freepascal.org/fpc/contrib/windows/
  or from ftp://ftp.freepascal.org/fpc/contrib/windows/
  and unzip it.

  In the unzip folder go to Win32 or Win64 folder
  get the dblib.dll
  and the libiconv.dll
  and place them in the same folder as your app's executable.

  SEE: https://wiki.freepascal.org/mssqlconn }
  TSqlProviderMsSql = class(TSqlProvider)
  public
    constructor Create();
    destructor Destroy(); override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlProviderMySql }
  { 1. MySql
    Go to the folder where MySql is installed, find the libmysql.dll in the \lib subfolder
    and copy it the the folder of your app's executable.
    2. MariaDb
    Go to the folder where MariaDb is installed, find the libmariadb.dll in the \lib subfolder
    copy it the the folder of your app's executable, and rename it as libmysql.dll
  }
  TSqlProviderMySql = class(TSqlProvider)
    {
  protected
    function GetTablesSql(): string; override;
    function GetViewsSql(): string; override;
    function GetTableFieldsSql(): string; override;
    function GetViewFieldsSql(): string; override;
    function GetIndexesSql(): string; override;
    function GetTriggersSql(): string; override;
    function GetConstraintsSql(): string; override;
    function GetProceduresSql(): string; override;
    function GetSequencesSql(): string; override;
    }
  public
    constructor Create();
    destructor Destroy(); override;

    { Returns the current date and time of the database serve }
    function GetServerDateTime(Store: TSqlStore): TDateTime; override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlProviderPostgreSql }

  TSqlProviderPostgreSql = class(TSqlProvider)
  public
    constructor Create();
    destructor Destroy(); override;

    { Returns the current date and time of the database serve }
    function GetServerDateTime(Store: TSqlStore): TDateTime; override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlProviderOracle }

  TSqlProviderOracle = class(TSqlProvider)
  public
    constructor Create();
    destructor Destroy(); override;

    { Returns the current date and time of the database serve }
    function GetServerDateTime(Store: TSqlStore): TDateTime; override;
    { Quotes and formats a date value as a string, properly for use with an Sql statement }
    function  QSDate(Value: TDateTime): string; override;
    { Quotes and formats a date-time value as a string, properly for use with an Sql statement }
    function  QSDateTime(Value: TDateTime): string; override;

    function  SelectTop(TableName: string; RowCount: Word = 400): string; override;
  end;

  { TSqlExec }
  TSqlExec = class(TComponent)
  private
    FConnectionInfo : TSqlConnectionInfo;
    FSqlConnector: TSQLConnectorEx;
    FSqlProvider: TSqlProvider;
    FSqlQuery: TSQLQuery;
    FSqlTransaction: TSQLTransaction;
    function GetInTransaction: Boolean;
  public
    constructor Create(ProviderType: TSqlProviderType; ConnectionString: string; AOwner: TComponent = nil); overload;
    destructor Destroy(); override;

    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();

    property SqlConnector: TSQLConnectorEx read FSqlConnector;
    property SqlTransaction: TSQLTransaction read FSqlTransaction;
    property SqlQuery: TSQLQuery read FSqlQuery;

    property InTransaction: Boolean read GetInTransaction;
    property SqlProvider: TSqlProvider read FSqlProvider;

    { e.g. Server=localhost; Database=C:\Path\To\DB.FDB; User=SYSDBA; Psw=masterkey; Charset=UTF8
      e.g. Server=localhost; Database=AdventureWorksLT2012; User=sa; Psw=p@$W03d; TrustServerCertificate=true }
    property ConnectionInfo : TSqlConnectionInfo read FConnectionInfo;
  end;

  { TSqlStore }
  TSqlStore = class
  private
    FConnectionInfo  : TSqlConnectionInfo;
    FProvider        : TSqlProvider;

    FSqlConnector    : TSQLConnectorEx;
    FSqlQuery        : TSQLQuery;
    FSqlTransaction  : TSQLTransaction;

    function GetConnectionName: string;
    function GetInTransaction: Boolean;
  protected
    procedure HandleException(Ex: Exception; SqlText: string);

    procedure CreateConnection();
    procedure FreeConnection();

    procedure PrepareCommand(const SqlText: string; Params: array of const); virtual;
    procedure UnPrepareCommand(); virtual;
  public
    constructor Create(ConnectionInfo: TSqlConnectionInfo);
    destructor Destroy(); override;

    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();

    function  CanConnect(ThrowIfNot: Boolean = false): Boolean;

    function  Select(const SqlText: string; Params: array of const): TMemTable; overload;
    function  Select(const SqlText: string): TMemTable; overload;

    procedure SelectTo(Table: TDataset; const SqlText: string; Params: array of const); overload;
    procedure SelectTo(Table: TDataset; const SqlText: string); overload;

    function  SelectResults(SqlText: string; const ResultFields: string; const Params: array of const): Variant; overload;
    function  SelectResults(SqlText: string; const ResultFields: string): Variant; overload;

    function  SelectResult(SqlText: string; Default: Variant; const Params: array of const): Variant; overload;
    function  SelectResult(SqlText: string; Default: Variant): Variant; overload;
    function  SelectResult(SqlText: string): Variant; overload;

    function  IntegerResult(SqlText: string; Default: Integer; const Params: array of const): Integer; overload;
    function  IntegerResult(SqlText: string; Default: Integer): Integer; overload;
    function  IntegerResult(SqlText: string): Integer; overload;

    procedure ExecSql(const SqlText: string; Params: array of const); overload;
    procedure ExecSql(const SqlText: string); overload;

    procedure GetNativeSchema(SchemaName, TableName, SqlText: string; Table: TDataset);

    property ConnectionName: string read GetConnectionName;
    property ConnectionInfo: TSqlConnectionInfo read FConnectionInfo;
    property Provider: TSqlProvider read FProvider;
    property SqlQuery: TSQLQuery read FSqlQuery;

    property InTransaction: Boolean read GetInTransaction;

  end;

  { DbSys }
  DbSys = class
  private
    class var
      FMetaDatabases: TMetaDatabases;

    class function GetBCDFieldTypes: TSetOfFieldType; static;
    class function GetBlobFieldTypess: TSetOfFieldType; static;
    class function GetDateTimeFieldTypes: TSetOfFieldType; static;
    class function GetFloatFieldTypes: TSetOfFieldType; static;
    class function GetIntegerFieldTypes: TSetOfFieldType; static;
    class function GetMetaDatabases: TMetaDatabases; static;
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

    class procedure AssignParams(Params: TParams; A: array of const); overload;
    class procedure AssignParams(Params: TParams; tblParams: TDataset); overload;
    class procedure AssignParams(Params: TParams; A: array of Variant); overload;
    class procedure AssignParams(Params: TParams; const ParamsDic: IDictionary<string, Variant>); overload;
    class procedure AssignStreamToParam(Param: TParam; Stream: TStream);
    class procedure AssignParam(Params: TParams; ParamName: string; Value: Variant);

    { properties }
    class property MetaDatabases: TMetaDatabases read GetMetaDatabases;
    class property StringFieldTypes  : TSetOfFieldType read GetStringFieldTypes;
    class property WideStringFieldTypes : TSetOfFieldType read GetWideStringFieldTypes;
    class property IntegerFieldTypes : TSetOfFieldType read GetIntegerFieldTypes;
    class property FloatFieldTypes : TSetOfFieldType read GetFloatFieldTypes;
    class property BCDFieldTypes : TSetOfFieldType read GetBCDFieldTypes;
    class property DateTimeFieldTypes : TSetOfFieldType read GetDateTimeFieldTypes;
    class property BlobFieldTypes  : TSetOfFieldType read GetBlobFieldTypess;
  end;

  { Sql }
  Sql = class
  public
  //static public bool IsMasked(string Value)
    class function IsMasked(Value: string): Boolean;
  end;


implementation



uses
   LResources
   ,Tripous.Logs
   ;


{ TSQLConnectorEx }
procedure TSQLConnectorEx.CreateProxy;
begin
  inherited CreateProxy;

  if Assigned(Proxy) then
     if Proxy is mysql80conn.TConnectionName then
        mysql80conn.TConnectionName(Proxy).SkipLibraryVersionCheck:= true;
end;




{ TMetaNode }
constructor TMetaNode.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string; ANodeType: TMetaNodeType);
var
  Parent: TMetaNode;
begin
  inherited Create();
  FParentNode := AParentNode;
  FName := AName;
  FSchemaName := ASchemaName;
  FNodeType := ANodeType;

  Parent := FParentNode;
  while Assigned(Parent) do
  begin
    if Parent is TMetaDatabase then
    begin
      FDatabase := Parent as TMetaDatabase;
      break;
    end;

    Parent := Parent.FParentNode;
  end;

  FIsContainer := not (FNodeType in [ntField, ntIndex, ntConstraint, ntTrigger, ntSequence]);
end;

function TMetaNode.GetDefinition(): string;
begin
  Result := '';
end;

function TMetaNode.GetParentTableOrView(): TMetaNode;
var
  Parent : TMetaNode;
begin
  Parent := FParentNode;
  while Assigned(Parent) do
  begin
    if (Parent is TMetaTable) or (Parent is TMetaView) then
    begin
      Result := Parent;
      break;
    end;

    Parent := Parent.FParentNode;
  end;
end;

function TMetaNode.GetNodes: TMetaNodeArray;
begin
  Result := [];
end;

procedure TMetaNode.DoClear();
begin
end;

function TMetaNode.GetDisplayText: string;
begin
  Result := FName;
end;

function TMetaNode.ListToArray(List: TGenObjectList<TMetaNode>): TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  if (List <> nil) and (List.Count > 0) then
  begin
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
      Result[i] := TMetaNode(List[i]);
  end;
end;

function TMetaNode.UnQuote(const S: string): string;
const
  A: array of char = ['''', '"', '[', ']', ' ', #9, #10, #11, #12, #13 ];
begin
   Result := S.Trim(A);
end;



{ TMetaField }
constructor TMetaField.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntField);
end;

function TMetaField.GetDefinition(): string;
begin
  Result := Name + ' ' + DataType;
  if not Sys.IsEmpty(DataSubType) then
     Result := Result + ' ' + DataSubType;

  if SizeInChars > 0 then
     Result := Result + '(' + IntToStr(SizeInChars) + ')';

  if (Precision > 0) and (Scale > 0) then
     Result := Result + '(' + IntToStr(Precision) + ', ' + IntToStr(Scale) + ')';

  if IsNullable then
     Result := Result + ' null'
  else
     Result := Result + ' not null';
end;

function TMetaField.GetText(NameOnly: Boolean): string;
begin
  if NameOnly then
    Result := Name
  else
    Result := DisplayText;
end;

function TMetaField.GetDisplayText: string;
begin

  Result := Name + ' (' + DataType;

  if not Sys.IsEmpty(DataSubType) then
     Result := Result + ' - ' + DataSubType;

  if SizeInChars > 0 then
     Result := Result + '(' + IntToStr(SizeInChars) + ')';

  if (Precision > 0) and (Scale > 0) then
     Result := Result + '(' + IntToStr(Precision) + ', ' + IntToStr(Scale) + ')';

  if IsNullable then
     Result := Result + ', null)'
  else
     Result := Result + ', not null)';

end;


{ TMetaFields }

constructor TMetaFields.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Fields', '', ntFields);
  FList := TGenObjectList<TMetaField>.Create(True, True);
end;

destructor TMetaFields.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TMetaFields.GetFieldListText(NamesOnly: Boolean): string;
var
  SB: IStringBuilder;
  Item : TMetaField;
begin
  SB := TStrBuilder.Create();
  for Item in FList do
     SB.AppendLine(Item.GetText(NamesOnly));

  Result := SB.ToUtf8String();
end;

procedure TMetaFields.DoClear();
begin
  FList.Clear();
end;

function TMetaFields.Add(AName: string; const ASchemaName: string): TMetaField;
begin
  Result := TMetaField.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaFields.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaFields.Find(const AName: string; const ASchemaName: string): TMetaField;
var
  Item : TMetaField;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

{ TMetaIndex }
constructor TMetaIndex.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntIndex);
end;

function TMetaIndex.GetDefinition(): string;
var
  MetaNode: TMetaNode;
  FieldList: string;
  Unique : string;
begin
  Result := '';
  MetaNode := GetParentTableOrView();
  if Assigned(MetaNode) and (MetaNode is TMetaTable) then
  begin
    FieldList := Sys.AsCommaText(Sys.Split(Fields, ';'));
    Unique := '';
    if IsUnique then
       Unique := 'unique';

    // create [unique] index INDEX_NAME on TABLE_NAME  (FIELD_LIST);
    Result := Format('create %s index %s on %s (%s)', [Unique, Name, MetaNode.Name, FieldList]);
  end;

end;

function TMetaIndex.GetDisplayText: string;
begin
  Result := Name;
  if not Sys.IsEmpty(Fields) then
     Result := Result + ' (' + Fields + ')';

  if not Sys.IsEmpty(IndexType) then
      Result := Result + ' ' + IndexType
  else if IsUnique then
      Result := Result + ' Unique';

end;



{ TMetaIndexes }

constructor TMetaIndexes.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Indexes', '', ntIndexes);
  FList := TGenObjectList<TMetaIndex>.Create(True, True);
end;

destructor TMetaIndexes.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaIndexes.DoClear();
begin
  FList.Clear();
end;

function TMetaIndexes.Add(AName: string; const ASchemaName: string): TMetaIndex;
begin
  Result := TMetaIndex.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaIndexes.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaIndexes.Find(const AName: string; const ASchemaName: string): TMetaIndex;
var
  Item : TMetaIndex;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

{ TMetaTrigger }

constructor TMetaTrigger.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
   inherited Create(AParentNode, AName, ASchemaName, ntTrigger);
end;

function TMetaTrigger.GetDefinition(): string;
begin
  Result := Definition;
end;

function TMetaTrigger.GetDisplayText: string;
begin
  Result := Name;
  if not Sys.IsEmpty(TriggerType) then
     Result := Result + ' (' + TriggerType + ')';
end;


{ TMetaTriggers }

constructor TMetaTriggers.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Triggers', '', ntTriggers);
  FList := TGenObjectList<TMetaTrigger>.Create(True, True);
end;

destructor TMetaTriggers.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaTriggers.DoClear();
begin
  FList.Clear();
end;

function TMetaTriggers.Add(AName: string; const ASchemaName: string): TMetaTrigger;
begin
  Result := TMetaTrigger.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaTriggers.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaTriggers.Find(const AName: string; const ASchemaName: string): TMetaTrigger;
var
  Item : TMetaTrigger;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

{ TMetaConstraint }
constructor TMetaConstraint.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntConstraint);
end;

function TMetaConstraint.GetDefinition(): string;
begin
  Result := '';
end;

function TMetaConstraint.GetDisplayText: string;
begin
  Result := Name;
  if not Sys.IsEmpty(Fields) then
     Result := Result + ' (' + Fields + ')';
  if not Sys.IsEmpty(ConstraintTypeText) then
     Result := Result + ' ' + ConstraintTypeText;
end;

{ TMetaConstraints }

constructor TMetaConstraints.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Constraints', '', ntConstraints);
  FList := TGenObjectList<TMetaConstraint>.Create(True, True);
end;

destructor TMetaConstraints.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaConstraints.DoClear();
begin
  FList.Clear();
  inherited DoClear();
end;

function TMetaConstraints.Add(AName: string; const ASchemaName: string): TMetaConstraint;
begin
  Result := TMetaConstraint.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaConstraints.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaConstraints.Find(const AName: string; const ASchemaName: string): TMetaConstraint;
var
  Item : TMetaConstraint;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;

end;

{ TMetaPrimaryKey }

constructor TMetaPrimaryKey.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName);
  FNodeType := ntPrimaryKey;
end;

function TMetaPrimaryKey.GetDefinition(): string;
var
  MetaNode: TMetaNode;
  FieldList: string;
begin
  Result := '';
  MetaNode := GetParentTableOrView();
  if Assigned(MetaNode) and (MetaNode is TMetaTable) then
  begin
    FieldList := Sys.AsCommaText(Sys.Split(Fields, ';'));
    Result := Format('alter table %s add constraint %s primary key (%s)', [MetaNode.Name, Name, FieldList]);
  end;

end;


{ TMetaPrimaryKeys }
constructor TMetaPrimaryKeys.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'PrimaryKeys', '', ntPrimaryKeys);
  FList := TGenObjectList<TMetaPrimaryKey>.Create(True, True);
end;

destructor TMetaPrimaryKeys.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaPrimaryKeys.DoClear();
begin
  FList.Clear();
  inherited DoClear();
end;

function TMetaPrimaryKeys.Add(AName: string; const ASchemaName: string): TMetaPrimaryKey;
begin
  Result := TMetaPrimaryKey.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaPrimaryKeys.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaPrimaryKeys.Find(const AName: string; const ASchemaName: string): TMetaPrimaryKey;
var
  Item : TMetaPrimaryKey;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;

end;



{ TMetaForeignKey }
constructor TMetaForeignKey.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName);
  FNodeType := ntForeignKey;
end;

function TMetaForeignKey.GetDefinition(): string;
var
  MetaNode: TMetaNode;
  FieldList: string;
  ForeignFieldList: string;
begin
  Result := '';

  MetaNode := GetParentTableOrView();
  if Assigned(MetaNode) and (MetaNode is TMetaTable) then
  begin
    FieldList := Sys.AsCommaText(Sys.Split(Fields, ';'));
    ForeignFieldList := Sys.AsCommaText(Sys.Split(ForeignFields, ';'));

    // alter table TABLE_NAME add constraint CONSTRAINT_NAME foreign key (FIELD_LIST) references FOREIGN_TABLE(FOREIGN_FIELD_LIST);
    Result := Format('alter table %s add constraint %s foreign key (%s) references %s(%s)',
                     [MetaNode.Name, Name, FieldList, ForeignTable, ForeignFieldList]);
  end;

end;

function TMetaForeignKey.GetDisplayText: string;
begin
  Result := Name;
  if not Sys.IsEmpty(Fields) then
     Result := Result + ' (' + Fields + ')';
  if not Sys.IsEmpty(ForeignTable) then
  begin
     Result := Result + ' references ' + ForeignTable;
     if not Sys.IsEmpty(ForeignFields) then
       Result := Result + ' (' + ForeignFields + ')';
  end;
end;

{ TMetaForeignKeys }
constructor TMetaForeignKeys.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'ForeignKeys', '', ntForeignKeys);
  FList := TGenObjectList<TMetaForeignKey>.Create(True, True);
end;

destructor TMetaForeignKeys.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaForeignKeys.DoClear();
begin
  FList.Clear();
  inherited DoClear();
end;

function TMetaForeignKeys.Add(AName: string; const ASchemaName: string): TMetaForeignKey;
begin
  Result := TMetaForeignKey.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaForeignKeys.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaForeignKeys.Find(const AName: string; const ASchemaName: string): TMetaForeignKey;
var
  Item : TMetaForeignKey;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;

end;


{ TMetaTable }
constructor TMetaTable.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntTable);
  FFields             := TMetaFields.Create(Self);
  FPrimaryKeys        := TMetaPrimaryKeys.Create(Self);
  FForeignKeys        := TMetaForeignKeys.Create(Self);
  FIndexes            := TMetaIndexes.Create(Self);
  FTriggers           := TMetaTriggers.Create(Self);
  FConstraints        := TMetaConstraints.Create(Self);
end;

destructor TMetaTable.Destroy();
begin
  FConstraints.Free();
  FTriggers.Free();
  FIndexes.Free();
  FForeignKeys.Free();
  FPrimaryKeys.Free();
  FFields.Free();
  inherited Destroy();
end;

function TMetaTable.GetDefinition(): string;
var
  SB: IStringBuilder;
  i : Integer;
  S : string;
begin
  SB := TStrBuilder.Create();

  SB.AppendLine(Format('create table %s (', [Name]));
  for i := 0 to Fields.List.Count - 1 do
  begin
    S := Fields.List[i].GetDefinition();
    if i <> Fields.List.Count - 1 then
      S := S + ', ';
    SB.AppendLine('  ' + S);
  end;
  SB.AppendLine(')');

  Result := SB.ToUtf8String();
end;

procedure TMetaTable.DoClear();
begin
  FConstraints.DoClear();
  FTriggers.DoClear();
  FIndexes.DoClear();
  FForeignKeys.DoClear();
  FPrimaryKeys.DoClear();
  FFields.DoClear();
end;

function TMetaTable.GetNodes: TMetaNodeArray;
begin
  Result := [];
  SetLength(Result, 6);
  Result[0] := Fields;
  Result[1] := PrimaryKeys;
  Result[2] := ForeignKeys;
  Result[3] := Constraints;
  Result[4] := Indexes;
  Result[5] := Triggers;
end;

{ TMetaTables }

constructor TMetaTables.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Tables', '', ntTables);
  FList := TGenObjectList<TMetaTable>.Create(True, True);
end;

destructor TMetaTables.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TMetaTables.Find(const AName: string; const ASchemaName: string): TMetaTable;
var
  Item: TMetaTable;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

function TMetaTables.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

procedure TMetaTables.DoClear();
begin
  FList.Clear();
end;

function TMetaTables.Add(AName: string; const ASchemaName: string): TMetaTable;
begin
  Result := TMetaTable.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;





{ TMetaView }

constructor TMetaView.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntView);
  FFields := TMetaFields.Create(Self);
end;

destructor TMetaView.Destroy();
begin
  FFields.Free();
  inherited Destroy();
end;

function TMetaView.GetDefinition(): string;
begin
  Result := Definition;
end;

procedure TMetaView.DoClear();
begin
  FFields.DoClear();
end;

function TMetaView.GetNodes: TMetaNodeArray;
begin
  Result := [];
  SetLength(Result, 1);
  Result[0] := Fields;
end;


{ TMetaViews }
constructor TMetaViews.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Views', '', ntViews);
  FList := TGenObjectList<TMetaView>.Create(True, True);
end;

destructor TMetaViews.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaViews.DoClear();
begin
  FList.Clear();
end;

function TMetaViews.Find(const AName: string; const ASchemaName: string): TMetaView;
var
  Item: TMetaView;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

function TMetaViews.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaViews.Add(AName: string; const ASchemaName: string): TMetaView;
begin
  Result := TMetaView.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntProcedure);
end;

destructor TMetaProcedure.Destroy();
begin
  inherited Destroy();
end;

function TMetaProcedure.GetDefinition(): string;
begin
  Result := Definition;
end;

procedure TMetaProcedure.DoClear();
begin
end;

function TMetaProcedure.GetDisplayText: string;
begin
  Result := Name;
  if not Sys.IsEmpty(ProcedureType) then
     Result := Result + ' (' + ProcedureType + ')';
end;

{ TMetaProcedures }

constructor TMetaProcedures.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Procedures', '', ntProcedures);
  FList := TGenObjectList<TMetaProcedure>.Create(True, True);
end;

destructor TMetaProcedures.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaProcedures.DoClear();
begin
  FList.Clear();
end;

function TMetaProcedures.Add(AName: string; const ASchemaName: string): TMetaProcedure;
begin
  Result := TMetaProcedure.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaProcedures.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaProcedures.Find(const AName: string; const ASchemaName: string): TMetaProcedure;
var
  Item: TMetaProcedure;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;

end;

{ TMetaSequence }

constructor TMetaSequence.Create(AParentNode: TMetaNode; const AName: string; const ASchemaName: string);
begin
  inherited Create(AParentNode, AName, ASchemaName, ntSequence);
end;

destructor TMetaSequence.Destroy();
begin
  inherited Destroy();
end;

function TMetaSequence.GetDefinition(): string;
begin
  Result := '';
end;

procedure TMetaSequence.DoClear();
begin
end;

{ TMetaSequences }

constructor TMetaSequences.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Sequences', '', ntSequences);
  FList := TGenObjectList<TMetaSequence>.Create(True, True);
end;

destructor TMetaSequences.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaSequences.DoClear();
begin
  FList.Clear();
end;

function TMetaSequences.Add(AName: string; const ASchemaName: string): TMetaSequence;
begin
  Result := TMetaSequence.Create(Self, AName, ASchemaName);
  FList.Add(Result);
end;

function TMetaSequences.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaSequences.Find(const AName: string; const ASchemaName: string): TMetaSequence;
var
  Item: TMetaSequence;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) and Sys.IsSameText(ASchemaName, Item.SchemaName) then
      Exit(Item);
  end;
end;

{ TMetaDatabase }

constructor TMetaDatabase.Create(AParentNode: TMetaNode; ConnectionInfo: TSqlConnectionInfo);
begin
  inherited Create(AParentNode, ConnectionInfo.Name, '', ntDatabase);
  FDatabase := Self;
  FConnectionInfo := ConnectionInfo.Clone();

  FSqlStore := TSqlStore.Create(ConnectionInfo);
  FSqlProviderType := FSqlStore.Provider.ProviderType;

  FTables := TMetaTables.Create(Self);
  FViews  := TMetaViews.Create(Self);
  FProcedures := TMetaProcedures.Create(Self);
  FSequences := TMetaSequences.Create(Self);
end;

destructor TMetaDatabase.Destroy();
begin
  FSequences.Free();
  FProcedures.Free();
  FViews.Free();
  FTables.Free();
  FConnectionInfo.Free();
  FSqlStore.Free();
  inherited Destroy();
end;

procedure TMetaDatabase.DoClear();
begin
  FSequences.DoClear();
  FProcedures.DoClear();
  FViews.DoClear();
  FTables.DoClear();
end;

function TMetaDatabase.GetDisplayText: string;
begin
  Result := ConnectionInfo.Name + ' (' + ConnectionInfo.Provider + ')';
end;

procedure TMetaDatabase.Clear();
begin
  FLoading := True;
  try
    DoClear();
    FLoaded := False;
  finally
    FLoading := False;
  end;
end;

procedure TMetaDatabase.LoadField(tblSql: TMemTable; MetaField: TMetaField);
begin
  MetaField.FDataType        := tblSql.FieldByName('DataType').AsString.Trim();
  MetaField.FDataSubType     := tblSql.FieldByName('DataSubType').AsString.Trim();
  MetaField.FIsNullable      := tblSql.FieldByName('IsNullable').AsInteger = 1;
  MetaField.FSizeInChars     := tblSql.FieldByName('SizeInChars').AsInteger;
  MetaField.FSizeInBytes     := tblSql.FieldByName('SizeInBytes').AsInteger;
  MetaField.FPrecision       := tblSql.FieldByName('DecimalPrecision').AsInteger;
  MetaField.FScale           := tblSql.FieldByName('DecimalScale').AsInteger;
  MetaField.FDefaultValue    := tblSql.FieldByName('DefaultValue').AsString.Trim();
  MetaField.FExpression      := tblSql.FieldByName('Expression').AsString.Trim();
  MetaField.FOrdinalPosition := tblSql.FieldByName('OrdinalPosition').AsInteger;
end;
procedure TMetaDatabase.LoadTables();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  TableName: string;
  MetaTable: TMetaTable;
begin
  SqlText := SqlStore.Provider.TablesSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      // table, if not already in the list
      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName, Schema);
      if not Assigned(MetaTable) then
        MetaTable := Tables.Add(TableName, Schema);

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;

procedure TMetaDatabase.LoadTableFields();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  TableName: string;
  FieldName: string;
  MetaTable: TMetaTable;
  MetaField: TMetaField;
begin
  SqlText := SqlStore.Provider.TableFieldsSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      // table, if not already in the list
      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName, Schema);
      if not Assigned(MetaTable) then
        MetaTable := Tables.Add(TableName, Schema);

      // field
      FieldName := tblSql.FieldByName('FieldName').AsString;
      FieldName := UnQuote(FieldName);
      MetaField := MetaTable.Fields.Add(FieldName, Schema);
      LoadField(tblSql, MetaField);

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;

procedure TMetaDatabase.LoadViews();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  ViewName: string;
  MetaView: TMetaView;
begin
  SqlText := SqlStore.Provider.ViewsSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      // table, if not already in the list
      ViewName := tblSql.FieldByName('TableName').AsString;
      ViewName := UnQuote(ViewName);

      MetaView := Views.Find(ViewName, Schema);
      if not Assigned(MetaView) then
      begin
        MetaView := Views.Add(ViewName, Schema);
        MetaView.FDefinition := tblSql.FieldByName('Definition').AsString;
      end;

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;

procedure TMetaDatabase.LoadViewFields();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  ViewName: string;
  FieldName: string;
  MetaView: TMetaView;
  MetaField: TMetaField;
begin
  SqlText := SqlStore.Provider.ViewFieldsSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      // table, if not already in the list
      ViewName := tblSql.FieldByName('TableName').AsString;
      ViewName := UnQuote(ViewName);

      MetaView := Views.Find(ViewName, Schema);
      if not Assigned(MetaView) then
      begin
        MetaView := Views.Add(ViewName, Schema);
        MetaView.FDefinition := tblSql.FieldByName('Definition').AsString;
      end;

      // field
      FieldName := tblSql.FieldByName('FieldName').AsString;
      FieldName := UnQuote(FieldName);
      MetaField := MetaView.Fields.Add(FieldName, Schema);
      LoadField(tblSql, MetaField);

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;

procedure TMetaDatabase.LoadIndexes();
  // -------------------------------------------------------
  procedure LoadIndex(tblSql: TMemTable; MetaIndex: TMetaIndex);
  begin
    //MetaIndex.FFields         := tblSql.FieldByName('FieldName').AsString.Trim();
    MetaIndex.FIndexType      := tblSql.FieldByName('IndexType').AsString.Trim();
    MetaIndex.FIsUnique       := tblSql.FieldByName('IsUnique').AsBoolean;
  end;
  // -------------------------------------------------------
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  TableName: string;
  IndexName: string;
  FieldName: string;
  MetaTable: TMetaTable;
  MetaIndex: TMetaIndex;
begin
  SqlText := SqlStore.Provider.IndexesSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName, Schema);
      if Assigned(MetaTable) then
      begin
        IndexName := tblSql.FieldByName('IndexName').AsString;
        IndexName := UnQuote(IndexName);

        FieldName := tblSql.FieldByName('FieldName').AsString;
        FieldName := UnQuote(FieldName);

        MetaIndex := MetaTable.Indexes.Find(IndexName, Schema);
        if not Assigned(MetaIndex) then
        begin
          MetaIndex := MetaTable.Indexes.Add(IndexName, Schema);
          MetaIndex.FFields := FieldName;
          LoadIndex(tblSql, MetaIndex);
        end else begin
          if not Sys.IsEmpty(FieldName) then;
             MetaIndex.FFields := MetaIndex.FFields + ';' + FieldName;
        end;
      end;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;

procedure TMetaDatabase.LoadTriggers();
  // -------------------------------------------------------
  procedure LoadTrigger(tblSql: TMemTable; MetaTrigger: TMetaTrigger);
  begin
    MetaTrigger.FTableName     := UnQuote(tblSql.FieldByName('TableName').AsString.Trim());
    MetaTrigger.FTriggerType   := tblSql.FieldByName('TriggerType').AsString.Trim();
    //MetaTrigger.FIsActive      := not tblSql.FieldByName('IsInactive').AsBoolean;
    MetaTrigger.FDefinition    := tblSql.FieldByName('Definition').AsString.Trim();
  end;
  // -------------------------------------------------------
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  TableName: string;
  TriggerName: string;
  MetaTable: TMetaTable;
  MetaTrigger: TMetaTrigger;
begin
  SqlText := SqlStore.Provider.TriggersSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName, Schema);
      if Assigned(MetaTable) then
      begin
        TriggerName := tblSql.FieldByName('TriggerName').AsString;
        TriggerName := UnQuote(TriggerName);

        MetaTrigger := MetaTable.Triggers.Add(TriggerName, Schema);
        LoadTrigger(tblSql, MetaTrigger);
      end;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;
// -------------------------------------------------------
procedure TMetaDatabase.LoadProcedures();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  ProcedureName: string;
  MetaProcedure: TMetaProcedure;
begin
  SqlText := SqlStore.Provider.ProceduresSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      ProcedureName := tblSql.FieldByName('ProcedureName').AsString.Trim();
      ProcedureName := UnQuote(ProcedureName);

      MetaProcedure := Procedures.Add(ProcedureName, Schema);
      MetaProcedure.FProcedureType := tblSql.FieldByName('ProcedureType').AsString.Trim();
      MetaProcedure.FDefinition    := tblSql.FieldByName('Definition').AsString.Trim();
      //MetaProcedure.FIsValid       := tblSql.FieldByName('IsValid').AsBoolean;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;

procedure TMetaDatabase.LoadConstraints();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  TableName: string;
  ConstraintName: string;
  MetaTable: TMetaTable;
  MetaConstraint: TMetaConstraint;
  MetaForeignKey: TMetaForeignKey;

  ConstraintType: TConstraintType;
  FieldName: string;
  ForeignField: string;
begin
  SqlText := SqlStore.Provider.ConstraintsSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try

    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      TableName := tblSql.FieldByName('TableName').AsString.Trim();
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName, Schema);

      if Assigned(MetaTable) then
      begin
        ConstraintName := tblSql.FieldByName('ConstraintName').AsString.Trim();
        ConstraintName := UnQuote(ConstraintName);

        ConstraintType :=  TConstraintType(tblSql.FieldByName('ConstraintType').AsInteger);
        case ConstraintType of
           ctPrimaryKey: MetaConstraint := MetaTable.PrimaryKeys.Find(ConstraintName, Schema);
           ctForeignKey: MetaConstraint := MetaTable.ForeignKeys.Find(ConstraintName, Schema);
        else
           MetaConstraint := MetaTable.Constraints.Find(ConstraintName, Schema);
        end;

        if not Assigned(MetaConstraint) then
        begin
          case ConstraintType of
             ctPrimaryKey: MetaConstraint := MetaTable.PrimaryKeys.Add(ConstraintName, Schema);
             ctForeignKey: MetaConstraint := MetaTable.ForeignKeys.Add(ConstraintName, Schema);
          else
             MetaConstraint := MetaTable.Constraints.Add(ConstraintName, Schema);
          end;

          MetaConstraint.FConstraintTypeText := tblSql.FieldByName('ConstraintTypeText').AsString.Trim();
          MetaConstraint.FConstraintType     := TConstraintType(tblSql.FieldByName('ConstraintType').AsInteger);
          MetaConstraint.FFields             := tblSql.FieldByName('FieldName').AsString.Trim();

          if ConstraintType = ctForeignKey then
          begin
            MetaForeignKey := TMetaForeignKey(MetaConstraint);
            MetaForeignKey.FForeignTable  := tblSql.FieldByName('ForeignTable').AsString.Trim();
            MetaForeignKey.FForeignFields := tblSql.FieldByName('ForeignField').AsString.Trim();
          end;
        end else begin
          FieldName    := tblSql.FieldByName('FieldName').AsString.Trim();

          if not Sys.IsEmpty(FieldName) then
          begin
            if Sys.IsEmpty(MetaConstraint.FFields) then
               MetaConstraint.FFields := FieldName
            else
               MetaConstraint.FFields := MetaConstraint.FFields + ';' + FieldName;
          end;

            if ConstraintType = ctForeignKey then
            begin
              MetaForeignKey := TMetaForeignKey(MetaConstraint);

              ForeignField := tblSql.FieldByName('ForeignField').AsString.Trim();
              if not Sys.IsEmpty(ForeignField) then
              begin
                if Sys.IsEmpty(MetaForeignKey.ForeignFields) then
                   MetaForeignKey.FForeignFields := ForeignField
                else
                   MetaForeignKey.FForeignFields := MetaForeignKey.FForeignFields + ';' + ForeignField;
              end;
            end;

        end;
      end;

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;

procedure TMetaDatabase.LoadSequences();
var
  SqlText : string;
  tblSql: TMemTable;
  Schema: string;
  SequenceName: string;
  MetaSequence: TMetaSequence;
begin
  SqlText := SqlStore.Provider.SequencesSql;
  SqlText := ProcessReplacementsInMetadataSql(SqlText);

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      Schema    := UnQuote(tblSql.FieldByName('SchemaName').AsString);

      SequenceName := tblSql.FieldByName('SequenceName').AsString.Trim();
      SequenceName := UnQuote(SequenceName);

      MetaSequence := Sequences.Add(SequenceName, Schema);
      MetaSequence.FCurrentValue := tblSql.FieldByName('CurrentValue').AsInteger;
      MetaSequence.FInitialValue := tblSql.FieldByName('InitialValue').AsInteger;
      MetaSequence.FIncrementBy  := tblSql.FieldByName('IncrementBy').AsInteger;

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;

function TMetaDatabase.ProcessReplacementsInMetadataSql(SqlText: string): string;
begin
  Result := SqlText;
  case SqlStore.Provider.ProviderType of
    ptFirebird    : ;
    ptMsSql       : ;
    ptMySql       : Result := Result.Replace('@SCHEMA_NAME', SqlStore.ConnectionInfo.Database, [rfReplaceAll, rfIgnoreCase]);
    ptPostgreSql  : ;
    ptOracle      : ;
    ptSqlite      : ;
  end;
end;

// -------------------------------------------------------

procedure TMetaDatabase.Load();
begin
  if not FLoaded then
  begin
    FLoading := True;
    try
      DoClear();

      LoadTables();
      LoadTableFields();
      LoadViews();
      LoadViewFields();
      LoadIndexes();
      LoadTriggers();
      LoadProcedures();
      LoadConstraints();
      LoadSequences();

      {
      case SqlProviderType of
        ptFirebird   : LoadFirebird();
        ptSqlite     : LoadSqlite();
        ptMsSql      : LoadMsSql();
        ptMySql      : LoadMySql();
        ptPostgreSql : LoadPostgreSql();
        ptOracle     : LoadOracle();
      end;
      }

      FLoaded := True;
    finally
      FLoading := False;
    end;
  end;
end;

function TMetaDatabase.GetNodes: TMetaNodeArray;
begin
  Result := [];
  SetLength(Result, 4);
  Result[0] := Tables;
  Result[1] := Views;
  Result[2] := Procedures;
  Result[3] := Sequences;
end;




{ TMetaDatabases }

constructor TMetaDatabases.Create(AutoLoad: Boolean);
begin
  inherited Create(nil, 'Databases', '', ntDatabases);
  FIsAutoLoad := AutoLoad;
  FList := TGenObjectList<TMetaDatabase>.Create(True, True);
end;

destructor TMetaDatabases.Destroy();
begin
  FList.Free(); // this frees the objects too.
  inherited Destroy();
end;

procedure TMetaDatabases.Clear();
begin
  FList.Clear();
end;

procedure TMetaDatabases.ReLoad();
var
  A: TSqlConnectionInfoArray;
  i : Integer;
begin
  A := [];
  SetLength(A, FList.Count);
  for i := 0 to FList.Count - 1 do
    A[i] := FList[i].ConnectionInfo.Clone();

  FList.Clear();                                           // this frees the objects too.
  AddRange(A);                                                     // forces databases to call Load();

  for i := Low(A) to High(A) do                                    // dispose the TSqlConnectionInfo instances
     A[i].Free();
end;

function TMetaDatabases.Add(ConnectionInfo: TSqlConnectionInfo): TMetaDatabase;
begin
  Result := TMetaDatabase.Create(Self, ConnectionInfo);
  FList.Add(Result);
  if IsAutoLoad then
     Result.Load();
end;

procedure TMetaDatabases.AddRange(ConnectionInfoList: TSqlConnectionInfoArray);
var
  i : Integer;
begin
  for i := Low(ConnectionInfoList) to High(ConnectionInfoList) do
      Add(ConnectionInfoList[i]);
end;

procedure TMetaDatabases.Remove(ADatabase: TMetaDatabase);
begin
  FList.Remove(ADatabase); // this frees the object too.
end;

function TMetaDatabases.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;










type

   { SchemaCacheItem }

   TSchemaCacheItem = class
   private
     FConnectionName: string;
     FSchemaName: string;
     FSchemaTable: TDataset;
   public
     constructor Create(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
     destructor Destroy(); override;
   end;

   SchemaCache = class
   private
     class var List: TList;
   public
     class constructor Create();
     class destructor Destroy();

     class function Find(ConnectionName: string; SchemaName: string): TDataset;
     class procedure Add(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
   end;

class constructor SchemaCache.Create();
begin
  List := TList.Create();
end;

class destructor SchemaCache.Destroy();
begin
  Sys.ClearObjectList(List);
  List.Free();
end;

class function SchemaCache.Find(ConnectionName: string; SchemaName: string): TDataset;
var
  i : Integer;
  Item: TSchemaCacheItem;
begin
  for i := 0 to List.Count - 1 do
  begin
    Item := TSchemaCacheItem(List[i]);
    if Sys.IsSameText(Item.FConnectionName, ConnectionName) and Sys.IsSameText(Item.FSchemaName, SchemaName) then
       Exit(Item.FSchemaTable);
  end;

  Result := nil;
end;

class procedure SchemaCache.Add(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
var
  Item: TSchemaCacheItem;
begin
  Item := TSchemaCacheItem.Create(ConnectionName, SchemaName, SchemaTable);
  List.Add(Item);
end;

{ TSchemaCacheItem }

constructor TSchemaCacheItem.Create(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
begin
  FConnectionName := ConnectionName;
  FSchemaName := SchemaName;
  FSchemaTable := SchemaTable;
end;

destructor TSchemaCacheItem.Destroy();
begin
  FSchemaTable.Free();
  inherited Destroy();
end;




{ TSqlConnectionInfo }
constructor TSqlConnectionInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Inc(FCounter);
  FId := FCounter;
  FParams := TStringList.Create();
end;

destructor TSqlConnectionInfo.Destroy();
begin
  FParams.Free();
  inherited Destroy();
end;

procedure TSqlConnectionInfo.Assign(Source: TPersistent);
var
  Src: TSqlConnectionInfo;
begin
  Clear();
  if Source is TSqlConnectionInfo then
  begin
    Src := Source as TSqlConnectionInfo;

    Name                   := Src.Name                  ;
    Provider               := Src.Provider              ;
    AutoCreateGenerators   := Src.AutoCreateGenerators  ;
    ConnectionString       := Src.ConnectionString      ;
  end;
end;

function TSqlConnectionInfo.Clone(): TSqlConnectionInfo;
//var
//  JsonText: string;
begin
  Result := TSqlConnectionInfo.Create();
  Result.Assign(Self);

(*
Result.Name     := Name;
Result.Provider := Provider;
Result.AutoCreateGenerators := AutoCreateGenerators;
Result.ConnectionString := ConnectionString;
*)
  //JsonText := Json.Serialize(Self);
  //Json.Deserialize(Result, JsonText);
end;

procedure TSqlConnectionInfo.Clear();
begin
  Name                   := '';
  Provider               := '';
  AutoCreateGenerators   := False;
  ConnectionString       := '';
end;

function TSqlConnectionInfo.GetSqlProvider(): TSqlProvider;
begin
  Result := SqlProviders.FindSqlProvider(Provider);
end;

function TSqlConnectionInfo.GetConnectorType: string;
begin
  Result := GetSqlProvider().ConnectorType;
end;

procedure TSqlConnectionInfo.SetupConnection(SqlConnector: TSQLConnectorEx);
var
  SqlProvider : TSqlProvider;
begin
  SqlProvider := GetSqlProvider();
  SqlProvider.ActivateLibraryLoader();

  SqlConnector.ConnectorType := ConnectorType;
  SqlConnector.HostName := Server;
  SqlConnector.DatabaseName := Database;
  SqlConnector.UserName := UserName;
  SqlConnector.Password := Password;
  SqlConnector.CharSet := CharSet;
  SqlConnector.Params.Assign(Params);
end;

procedure TSqlConnectionInfo.SetConnectionString(AValue: string);
var
  List: TStrings;
  i : Integer;
  Key, Value: string;

begin

  FDatabase := '';
  FServer := '';
  FUserName := '';
  FPassword := '';
  FCharSet  := '';

  FParams.Clear();

  FConnectionString := AValue;

  if not Sys.IsEmpty(FConnectionString) then
  begin
    List := Sys.SplitToList(FConnectionString, ';');
    try
      for i := 0 to List.Count - 1 do
      begin
        Key   := List.Names[i];
        Value := Trim(List.Values[Key]);
        Key   := Trim(Key);

        if      Sys.IsSameText(Key, 'Provider')  then
          FProvider := Value
        else if Sys.IsSameText(Key, 'Database')
             or Sys.IsSameText(Key, 'DatabaseName')
             or Sys.IsSameText(Key, 'Database Name')
             or Sys.IsSameText(Key, 'Initial Catalog')  then
          FDatabase := Value
        else if Sys.IsSameText(Key, 'Host')
             or Sys.IsSameText(Key, 'HostName')
             or Sys.IsSameText(Key, 'Server')
             or Sys.IsSameText(Key, 'DataSource')
             or Sys.IsSameText(Key, 'Data Source') then
          FServer := Value
        else if Sys.IsSameText(Key, 'User')
             or Sys.IsSameText(Key, 'UserName')
             or Sys.IsSameText(Key, 'User Name')
             or Sys.IsSameText(Key, 'UserId')
             or Sys.IsSameText(Key, 'User Id')
             or Sys.IsSameText(Key, 'Uid') then
           FUserName := Value
        else if Sys.IsSameText(Key, 'Password')
             or Sys.IsSameText(Key, 'Psw')  then
           FPassword := Value
        else if Sys.IsSameText(Key, 'CharSet')  then
           FCharSet := Value
        else
           FParams.Add(List[i]);
      end;
    finally
      List.Free();
    end;

  end;

end;

function TSqlConnectionInfo.CanConnect(ThrowIfNot: Boolean): Boolean;
begin
  Result := GetSqlProvider().CanConnect(Self, ThrowIfNot);
end;

{ TSqlConnectionInfoListEnumerator }

function TSqlConnectionInfoListEnumerator.GetCurrent: TSqlConnectionInfo;
begin
    Result := FItemList[Position] as TSqlConnectionInfo;
end;



{ TSqlConnectionInfoList }

function TSqlConnectionInfoList.GetItem(Index: Integer): TSqlConnectionInfo;
begin
  Result := fSqlConnections.Items[Index] as TSqlConnectionInfo;
end;

function TSqlConnectionInfoList.GetCount: Integer;
begin
  Result := fSqlConnections.Count;
end;

constructor TSqlConnectionInfoList.Create();
begin
  inherited Create();
  fSqlConnections := TCollection.Create(TSqlConnectionInfo);
end;

destructor TSqlConnectionInfoList.Destroy();
begin
  fSqlConnections.Free();
  inherited Destroy();
end;

procedure TSqlConnectionInfoList.Assign(Source: TPersistent);
var
  Item:  TSqlConnectionInfo;
begin
  Clear();
  if Source is  TSqlConnectionInfoList then
    for Item in  TSqlConnectionInfoList(Source) do
      Add(Item.Clone());
end;

function TSqlConnectionInfoList.Clone(): TSqlConnectionInfoList;
begin
  Result := TSqlConnectionInfoList.Create();
  Result.Assign(Self);
end;

procedure TSqlConnectionInfoList.Clear();
begin
  fSqlConnections.Clear();
end;

procedure TSqlConnectionInfoList.Load(FilePath: string);
begin
  Clear();
  Json.LoadFromFile(FilePath, Self);
end;

procedure TSqlConnectionInfoList.Save(FilePath: string);
begin
  Json.SaveToFile(FilePath, Self);
end;

procedure TSqlConnectionInfoList.Remove(Item: TSqlConnectionInfo);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
     fSqlConnections.Delete(Index);
end;

procedure TSqlConnectionInfoList.Add(Item: TSqlConnectionInfo);
begin
  Item.Collection := fSqlConnections;
end;

function TSqlConnectionInfoList.IndexOf(Item: TSqlConnectionInfo): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to fSqlConnections.Count - 1 do
  begin
    if Item = fSqlConnections.Items[i] then
      exit(i);
  end;
end;

function TSqlConnectionInfoList.Contains(const Name: string): Boolean;
begin
  Result := Find(Name) <> nil;
end;

function TSqlConnectionInfoList.Find(const Name: string): TSqlConnectionInfo;
var
  i : Integer;
  ConInfo : TSqlConnectionInfo;
begin
  Result := nil;

  for i := 0 to fSqlConnections.Count - 1 do
  begin
    ConInfo := fSqlConnections.Items[i] as TSqlConnectionInfo;
    if Sys.IsSameText(Name, ConInfo.Name) then
      exit(ConInfo);
  end;
end;

function TSqlConnectionInfoList.GetEnumerator(): TSqlConnectionInfoListEnumerator;
begin
  Result := TSqlConnectionInfoListEnumerator.Create(Sys.CollectionToArray(Self.SqlConnections));
end;


{ TSqlConInfoProxy }

constructor TSqlConInfoProxy.Create();
begin
  inherited Create;
  FId := Sys.GenId(False);
  FParams := TStringList.Create();
  FProvider := SqlProviders.SSqlite;
end;

constructor TSqlConInfoProxy.CreateFromTable(Table: TDataset);
begin
  Create();
  LoadFromTable(Table);
end;

destructor TSqlConInfoProxy.Destroy();
begin
  FParams.Free();
  inherited Destroy();
end;

function TSqlConInfoProxy.CreateSqlParamList(): TStrings;
var
  List: TStringList;

  procedure Add(Key, Value: string);
  begin
    List.Add(Key + '=' + Value);
  end;

begin
  List := TStringList.Create();

  Add('Id', Id);

  Add('Name', Name);
  Add('Provider', Provider);

  Add('Server', Server);
  Add('Database', Database);
  Add('UserName', UserName);
  Add('Password', Password);
  Add('Params', Params.Text);

  Result := List;
end;

procedure TSqlConInfoProxy.ToSqlParams(SqlParams: TParams);
begin
  SqlParams.ParamByName('Id'         ).AsString   := Id          ;
  SqlParams.ParamByName('Name'       ).AsString   := Name        ;
  SqlParams.ParamByName('Provider'   ).AsString   := Provider    ;
  SqlParams.ParamByName('Server'     ).AsString   := Server      ;
  SqlParams.ParamByName('Database'   ).AsString   := Database    ;
  SqlParams.ParamByName('UserName'   ).AsString   := UserName    ;
  SqlParams.ParamByName('Password'   ).AsString   := Password    ;
  SqlParams.ParamByName('Params'     ).AsString   := Params.Text ;
end;

procedure TSqlConInfoProxy.ToSqlParams(Q: TSQLQuery);
begin
  ToSqlParams(Q.Params);
end;

function TSqlConInfoProxy.ToDictionary(): IDictionary<string, Variant>;
begin
  Result := TGenDictionary<string, Variant>.Create();

  Result['Id'      ] := Id          ;
  Result['Name'    ] := Name        ;
  Result['Provider'] := Provider    ;
  Result['Server'  ] := Server      ;
  Result['Database'] := Database    ;
  Result['UserName'] := UserName    ;
  Result['Password'] := Password    ;
  Result['Params'  ] := Params.Text ;
end;

function TSqlConInfoProxy.GetInsertIntoSql(): string;
begin
  Result :=
  'insert into Datastores ( ' +
  ' Id                      ' +
  ',Name                    ' +
  ',Provider                ' +
  ',Server                  ' +
  ',Database                ' +
  ',UserName                ' +
  ',Password                ' +
  ',Params                  ' +
  ') values (               ' +
  ' :Id                     ' +
  ',:Name                   ' +
  ',:Provider               ' +
  ',:Server                 ' +
  ',:Database               ' +
  ',:UserName               ' +
  ',:Password               ' +
  ',:Params                 ' +
  ')'
  ;
end;

function TSqlConInfoProxy.GetUpdateSql(): string;
begin
  Result :=
  'update Datastores set  ' +
  ' Name      = :Name     ' +
  ',Provider  = :Provider ' +
  ',Server    = :Server   ' +
  ',Database  = :Database ' +
  ',UserName  = :UserName ' +
  ',Password  = :Password ' +
  ',Params    = :Params   ' +
  'where                  ' +
  '  Id       = :Id       ' +
  ''
  ;
end;

function TSqlConInfoProxy.GetDeleteSql(): string;
begin
  Result :=
  'delete from Datastores ' +
  'where                  ' +
  '  Id       = :Id       ' +
  ''
  ;
end;

procedure TSqlConInfoProxy.FromConnectionInfo(SqlConInfo: TSqlConnectionInfo);
begin
  //Id       :=
  Name     := SqlConInfo.Name;
  Provider := SqlConInfo.Provider;
  Server   := SqlConInfo.Server;
  Database := SqlConInfo.Database;
  UserName := SqlConInfo.UserName;
  Password := SqlConInfo.Password;
  Params.Clear();
  Params.Text := SqlConInfo.Params.Text;
end;

function TSqlConInfoProxy.CreateSqlConnectionInfo(): TSqlConnectionInfo;
begin
  Result := TSqlConnectionInfo.Create(nil);
  ToConnectionInfo(Result);
end;

procedure TSqlConInfoProxy.ToConnectionInfo(SqlConInfo: TSqlConnectionInfo);
begin
  SqlConInfo.Name := Name;
  SqlConInfo.Provider := Provider;
  SqlConInfo.ConnectionString := ToConnectionString();
end;

function TSqlConInfoProxy.ToConnectionString(): string;
var
  List: TStringList;
  A : TStringDynArray;
  S: string;

  procedure Add(Key: string; Value: string);
  begin
    if not Sys.IsEmpty(Value) then
       List.Add(Key + '=' + Value);
  end;

begin
  List := TStringList.Create();
  try
    Add('Server', Server);
    Add('Database', Database);
    Add('UserName', UserName);
    Add('Password', Password);
    if Assigned(Params) then
    begin
      for S in Params do
        List.Add(S);
    end;

    A := List.ToStringArray;
    Result := string.Join(';', A);

  finally
    List.Free();
  end;

end;

procedure TSqlConInfoProxy.LoadFromTable(Table: TDataset);
begin
  Id       := Table.FieldByName('Id'      ).AsString;
  Name     := Table.FieldByName('Name'    ).AsString;
  Provider := Table.FieldByName('Provider').AsString;
  Server   := Table.FieldByName('Server'  ).AsString;
  Database := Table.FieldByName('Database').AsString;
  UserName := Table.FieldByName('UserName').AsString;
  Password := Table.FieldByName('Password').AsString;
  Params.Clear();
  Params.Text := Table.FieldByName('Params').AsString;
end;

{ SqlProviders }

class constructor SqlProviders.Create();
begin
  Providers := TGenObjectList<TSqlProvider>.Create(True, True); // TList.Create();

  Providers.Add(TSqlProviderFirebird.Create());
  Providers.Add(TSqlProviderMsSql.Create());
  Providers.Add(TSqlProviderMySql.Create());
  Providers.Add(TSqlProviderPostgreSql.Create());
  Providers.Add(TSqlProviderOracle.Create());
  Providers.Add(TSqlProviderSqlite.Create());
end;

class destructor SqlProviders.Destroy();
begin
  //Sys.ClearObjectList(Providers);
  //Providers.Free();
end;

class function SqlProviders.ProviderTypeToString(ProviderType: TSqlProviderType): string;
var
  i: Integer;
begin
  for i := Low(ProviderTypes) to High(ProviderTypes) do
      if ProviderType = ProviderTypes[i] then
         Exit(ProviderNames[i]);

  Result := '';
end;

class function SqlProviders.StringToProviderType(ProviderType: string): TSqlProviderType;
var
  i: Integer;
begin
  for i := Low(ProviderNames) to High(ProviderNames) do
      if Sys.IsSameText(ProviderType, ProviderNames[i]) then
         Exit(ProviderTypes[i]);

  Result := ptNone;
end;

class function SqlProviders.FindSqlProvider(const ProviderName: string): TSqlProvider;
var
  Item : Pointer;
begin
  Result := nil;
  for Item in Providers do
    if Sys.IsSameText(ProviderName, TSqlProvider(Item).Name) then
      exit(TSqlProvider(Item));
end;

{ TSqlProvider }
constructor TSqlProvider.Create(ProviderType: TSqlProviderType);
begin
  inherited Create();
  FProviderType := ProviderType;
  FName         := ProviderTypeToProviderName(ProviderType);

  // https://forum.lazarus.freepascal.org/index.php?topic=27379.0
  FLibraryLoader := TSQLDBLibraryLoader.Create(nil);
  FLibraryLoader.ConnectionType := ConnectorType;
end;

destructor TSqlProvider.Destroy();
begin
  FLibraryLoader.Free();
  inherited Destroy();
end;

class function TSqlProvider.ProviderTypeToProviderName(ProviderType: TSqlProviderType): string;
begin
  Result := 'Unknown';

  case ProviderType of
    ptFirebird    : Result := SqlProviders.SFirebird  ;
    ptMsSql       : Result := SqlProviders.SMsSql     ;
    ptMySql       : Result := SqlProviders.SMySql     ;
    ptPostgreSql  : Result := SqlProviders.SPostgreSql;
    ptOracle      : Result := SqlProviders.SOracle    ;
    ptSqlite      : Result := SqlProviders.SSqlite    ;
  end;
end;

class function TSqlProvider.ProviderTypeToConnectorType(ProviderType: TSqlProviderType): string;
begin
  Result := 'Unknown';

  case ProviderType of
    ptFirebird    : Result := 'Firebird'     ;
    ptMsSql       : Result := 'MSSQLServer'  ;
    ptMySql       : Result := 'MySQL 8.0'    ;
    ptPostgreSql  : Result := 'PostgreSQL'   ;
    ptOracle      : Result := 'Oracle'       ;
    ptSqlite      : Result := 'SQLite3'      ;
  end;
end;

class function TSqlProvider.ProviderNameToProviderType(ProviderName: string): TSqlProviderType;
begin
  Result := ptNone;

  if not Sys.IsEmpty(ProviderName) then
  begin
         if Sys.IsSameText(ProviderName, SqlProviders.SFirebird   ) then  Result := ptFirebird
    else if Sys.IsSameText(ProviderName, SqlProviders.SMsSql      ) then  Result := ptMsSql
    else if Sys.IsSameText(ProviderName, SqlProviders.SMySql      ) then  Result := ptMySql
    else if Sys.IsSameText(ProviderName, SqlProviders.SPostgreSql ) then  Result := ptPostgreSql
    else if Sys.IsSameText(ProviderName, SqlProviders.SOracle     ) then  Result := ptOracle
    else if Sys.IsSameText(ProviderName, SqlProviders.SSqlite     ) then  Result := ptSqlite
  end;
end;

class function TSqlProvider.ProviderNameToConnectorType(ProviderName: string): string;
begin
  Result := 'Unknown';
  if not Sys.IsEmpty(ProviderName) then
  begin
         if Sys.IsSameText(ProviderName, SqlProviders.SFirebird   ) then  Result := 'Firebird'
    else if Sys.IsSameText(ProviderName, SqlProviders.SMsSql      ) then  Result := 'MSSQLServer'
    else if Sys.IsSameText(ProviderName, SqlProviders.SMySql      ) then  Result := 'MySQL 8.0'
    else if Sys.IsSameText(ProviderName, SqlProviders.SPostgreSql ) then  Result := 'PostgreSQL'
    else if Sys.IsSameText(ProviderName, SqlProviders.SOracle     ) then  Result := 'Oracle'
    else if Sys.IsSameText(ProviderName, SqlProviders.SSqlite     ) then  Result := 'SQLite3'
  end;
end;

procedure TSqlProvider.ActivateLibraryLoader();
begin
  if not FLibraryLoader.Enabled then
     FLibraryLoader.Enabled := True;
end;

function TSqlProvider.GetConnectorType: string;
begin
  Result := ProviderTypeToConnectorType(ProviderType);
end;

function TSqlProvider.CanConnect(ConnectionString: string; ThrowIfNot: Boolean): Boolean;
var
  ConInfo: TSqlConnectionInfo;
begin
  ConInfo:= TSqlConnectionInfo.Create(nil);
  try
    ConInfo.Name := 'CanConnect';
    ConInfo.Provider := Self.Name;
    ConInfo.ConnectionString := ConnectionString;
    Result := CanConnect(ConInfo, ThrowIfNot);
  finally
    ConInfo.Free();
  end;
end;

function TSqlProvider.CanConnect(ConInfo: TSqlConnectionInfo; ThrowIfNot: Boolean): Boolean;
var
  SqlStore: TSqlStore;
begin
  SqlStore := TSqlStore.Create(ConInfo);
  try
    Result := SqlStore.CanConnect(ThrowIfNot);
  finally
    SqlStore.Free();
  end;
end;

function TSqlProvider.GetServerDateTime(Store: TSqlStore): TDateTime;
begin
  Result := NowUTC();  // LocalTimeToUniversal(Now);
end;

function TSqlProvider.QSDate(Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', Value);
  Result := Sys.QS(Result);
end;

function TSqlProvider.QSDateTime(Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
  Result := Sys.QS(Result);
end;



function TSqlProvider.GetTablesSql(): string;
var
  ResourceName: string;
begin
  if FTablesSql = '' then
  begin
    ResourceName := Self.Name + '_Tables';
    FTablesSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FTablesSql;
end;

function TSqlProvider.GetViewsSql(): string;
var
  ResourceName: string;
begin
  if FViewsSql = '' then
  begin
    ResourceName := Self.Name + '_Views';
    FViewsSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FViewsSql;
end;

function TSqlProvider.GetTableFieldsSql(): string;
var
  ResourceName: string;
begin
  if FTableFieldsSql = '' then
  begin
    ResourceName := Self.Name + '_TableFields';
    FTableFieldsSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FTableFieldsSql;
end;

function TSqlProvider.GetViewFieldsSql(): string;
var
  ResourceName: string;
begin
  if FViewFieldsSql = '' then
  begin
    ResourceName := Self.Name + '_ViewFields';
    FViewFieldsSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FViewFieldsSql;
end;

function TSqlProvider.GetIndexesSql(): string;
var
  ResourceName: string;
begin
  if FIndexesSql = '' then
  begin
    ResourceName := Self.Name + '_Indexes';
    FIndexesSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FIndexesSql;
end;

function TSqlProvider.GetTriggersSql(): string;
var
  ResourceName: string;
begin
  if FTriggersSql = '' then
  begin
    ResourceName := Self.Name + '_Triggers';
    FTriggersSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FTriggersSql;
end;

function TSqlProvider.GetConstraintsSql(): string;
var
  ResourceName: string;
begin
  if FConstraintsSql = '' then
  begin
    ResourceName := Self.Name + '_Constraints';
    FConstraintsSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FConstraintsSql;
end;

function TSqlProvider.GetProceduresSql(): string;
var
  ResourceName: string;
begin
  if FProceduresSql = '' then
  begin
    ResourceName := Self.Name + '_Procedures';
    FProceduresSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FProceduresSql;
end;

function TSqlProvider.GetSequencesSql(): string;
var
  ResourceName: string;
begin
  if FSequencesSql = '' then
  begin
    ResourceName := Self.Name + '_Sequences';
    FSequencesSql := Sys.LoadResourceTextFile(ResourceName);
  end;
  Result := FSequencesSql;
end;



{ TSqlProviderFirebird }

constructor TSqlProviderFirebird.Create();
begin
  inherited Create(ptFirebird);
end;

destructor TSqlProviderFirebird.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderFirebird.GetServerDateTime(Store: TSqlStore): TDateTime;
begin
  Result := inherited GetServerDateTime(Store);
  Result := Store.SelectResult('select current_timestamp from rdb$database', Result);
end;

function TSqlProviderFirebird.SelectTop(TableName: string; RowCount: Word): string;
begin
  // select first N * from TABLE_NAME
  Result := string.Format('select first %d * from %s', [RowCount, TableName]);
end;

{ TSqlProviderSqlite }

constructor TSqlProviderSqlite.Create();
begin
  inherited Create(ptSqlite);
end;

destructor TSqlProviderSqlite.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderSqlite.SelectTop(TableName: string; RowCount: Word): string;
begin
  //  select * from TABLE_NAME limit N
  Result := string.Format('select * from %s limit %d', [TableName, RowCount]);
end;

{ TSqlProviderMsSql }

constructor TSqlProviderMsSql.Create();
begin
  inherited Create(ptMsSql);
end;

destructor TSqlProviderMsSql.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderMsSql.SelectTop(TableName: string; RowCount: Word): string;
begin
  // select top N * from TABLE_NAME
  Result := string.Format('select top %d * from %s', [RowCount, TableName]);
end;

{ TSqlProviderMySql }

constructor TSqlProviderMySql.Create();
begin
  inherited Create(ptMySql);
end;

destructor TSqlProviderMySql.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderMySql.GetServerDateTime(Store: TSqlStore): TDateTime;
begin
  Result := inherited GetServerDateTime(Store);
  Result := Store.SelectResult('select current_timestamp', Result);
end;

function TSqlProviderMySql.SelectTop(TableName: string; RowCount: Word): string;
begin
  // select * from TABLE_NAME limit N
  Result := string.Format('select * from %s limit %d', [TableName, RowCount]);
end;


{ TSqlProviderPostgreSql }
constructor TSqlProviderPostgreSql.Create();
begin
  inherited Create(ptPostgreSql);
end;

destructor TSqlProviderPostgreSql.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderPostgreSql.GetServerDateTime(Store: TSqlStore): TDateTime;
begin
  Result := inherited GetServerDateTime(Store);
  Result := Store.SelectResult('select current_timestamp', Result);
end;

function TSqlProviderPostgreSql.SelectTop(TableName: string; RowCount: Word): string;
begin
  // select * from TABLE_NAME limit N
  Result := string.Format('select * from %s limit %d', [TableName, RowCount]);
end;

{ TSqlProviderOracle }

constructor TSqlProviderOracle.Create();
begin
  inherited Create(ptOracle);
end;

destructor TSqlProviderOracle.Destroy();
begin
  inherited Destroy();
end;

function TSqlProviderOracle.GetServerDateTime(Store: TSqlStore): TDateTime;
var
  SqlText: string;
begin
  Result  := inherited GetServerDateTime(Store);
  SqlText := 'SELECT TO_CHAR(SYSDATE, ''YYYY-MM-DD HH24:MI:SS'') FROM Dual'; // select current_timestamp  FROM dual
  Result  := Store.SelectResult(SqlText, Result);
end;

function TSqlProviderOracle.QSDate(Value: TDateTime): string;
var
  S : string;
begin
  S := FormatDateTime('yyyy-mm-dd', Value);
  Result := Format('to_date(''%s'', ''YYYY-MM-DD'')', [S]);
end;

function TSqlProviderOracle.QSDateTime(Value: TDateTime): string;
var
  S : string;
begin
  S := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
  Result := Format('to_date(''%s'', ''YYYY-MM-DD:HH24:MI:SS'')', [S]);
end;

function TSqlProviderOracle.SelectTop(TableName: string; RowCount: Word): string;
begin
  // select * from TABLE_NAME where ROWNUM <= N
  Result := string.Format('select * from %s where ROWNUM <= %d', [TableName, RowCount]);
end;




{ TSqlExec }
constructor TSqlExec.Create(ProviderType: TSqlProviderType; ConnectionString: string; AOwner: TComponent);
begin
  inherited Create(AOwner);

  FConnectionInfo := TSqlConnectionInfo.Create();
  FConnectionInfo.Provider := TSqlProvider.ProviderTypeToProviderName(ProviderType);
  FConnectionInfo.ConnectionString := ConnectionString;

  FSqlProvider    := FConnectionInfo.GetSqlProvider();

  FSqlConnector   := TSQLConnectorEx.Create(Self);
  FSqlTransaction := TSQLTransaction.Create(Self);
  FSqlQuery       := TSQLQuery.Create(Self);

  FSqlConnector.KeepConnection := False;
  FConnectionInfo.SetupConnection(FSqlConnector);

  FSqlTransaction.Options := [TSQLTransactionOption.stoExplicitStart];     // TSQLTransactionOption = (stoUseImplicit, stoExplicitStart);

  FSqlTransaction.DataBase  := FSqlConnector;
  //FSqlConnector.Transaction := FSqlTransaction;
  FSqlQuery.Transaction     := FSqlTransaction;
  FSqlQuery.DataBase        := FSqlConnector;
end;

destructor TSqlExec.Destroy();
begin
  FConnectionInfo.Free();
  inherited Destroy();
end;

function TSqlExec.GetInTransaction: Boolean;
begin
  Result := FSqlConnector.Connected and FSqlTransaction.Active;
end;

procedure TSqlExec.StartTransaction();
begin
  if not InTransaction then
  begin
    FSqlConnector.Connected := True;
    FSqlTransaction.StartTransaction();
  end;
end;

procedure TSqlExec.Commit();
begin
  if InTransaction then
  begin
    FSqlTransaction.Commit();
    FSqlConnector.Connected := False;
  end;
end;

procedure TSqlExec.Rollback();
begin
  if InTransaction then
  begin
    FSqlTransaction.Rollback();
    FSqlConnector.Connected := False;
  end;
end;


{ TSqlStore }
constructor TSqlStore.Create(ConnectionInfo: TSqlConnectionInfo);
begin
  inherited Create();
  FConnectionInfo := ConnectionInfo.Clone();
  FProvider       := FConnectionInfo.GetSqlProvider();
end;

destructor TSqlStore.Destroy();
begin
  FConnectionInfo.Free();
  inherited Destroy();
end;

procedure TSqlStore.CreateConnection();
begin
  if Assigned(FSqlQuery) then
     FreeConnection();

  FSqlConnector   := TSQLConnectorEx.Create(nil);
  FSqlTransaction := TSQLTransaction.Create(nil);
  FSqlQuery       := TSQLQuery.Create(nil);

  FSqlConnector.KeepConnection := False;
  FConnectionInfo.SetupConnection(FSqlConnector);

  FSqlTransaction.DataBase  := FSqlConnector;
  FSqlConnector.Transaction := FSqlTransaction;
  //FSqlQuery.Transaction     := FSqlTransaction;
  FSqlQuery.DataBase        := FSqlConnector;
end;

procedure TSqlStore.FreeConnection();
begin
  if Assigned(FSqlQuery) then
  begin
    FSqlQuery.Active := False;
    FSqlTransaction.Active := False;
    FSqlConnector.Connected := False;

    FSqlQuery.Free();
    FSqlTransaction.Free();
    FSqlConnector.Free();

    FSqlQuery := nil;
    FSqlTransaction := nil;
    FSqlConnector := nil;
  end;
end;

procedure TSqlStore.StartTransaction();
begin
  CreateConnection();

  //if FSqlTransaction.Active then
  //  Sys.Error('Transaction is already started.');

  FSqlConnector.Connected := True;
  FSqlTransaction.StartTransaction();
end;

procedure TSqlStore.Commit();
begin
  if not FSqlTransaction.Active then
    Sys.Error('Transaction is not active.');

  //FSqlTransaction.Active := False;
  FSqlTransaction.Commit();
  FSqlConnector.Connected := False;

  FreeConnection();
end;

procedure TSqlStore.Rollback();
begin
  if not FSqlTransaction.Active then
    Sys.Error('Transaction is not active.');

  FSqlTransaction.Rollback();
  FSqlConnector.Connected := False;

  FreeConnection();
end;

function TSqlStore.GetInTransaction: Boolean;
begin
  Result := Assigned(FSqlConnector)
        and Assigned(FSqlTransaction)
        and FSqlConnector.Connected
        and FSqlTransaction.Active;
end;

function TSqlStore.CanConnect(ThrowIfNot: Boolean): Boolean;
begin
  Result := False;
  try
    CreateConnection();
    try
      FSqlConnector.Connected := True;
      FSqlConnector.Connected := False;
      Result := True;
    finally
      FreeConnection();
    end;
  except
    on Ex: Exception do
    begin
      Logger.Error(Ex);
      if ThrowIfNot then
        raise;
    end;
  end;
end;

procedure TSqlStore.PrepareCommand(const SqlText: string; Params: array of const);
begin
  UnPrepareCommand();

  if FSqlQuery.SQL.Text <> SqlText then
    FSqlQuery.SQL.Text := SqlText;

  if not FSqlQuery.Prepared then;
     FSqlQuery.Prepare();

  if (FSqlQuery.Params.Count > 0) and (Length(Params) > 0) then
     DbSys.AssignParams(FSqlQuery.Params, Params);
end;

procedure TSqlStore.UnPrepareCommand();
begin
  FSqlQuery.Active := False;
  if FSqlQuery.Prepared then
     FSqlQuery.UnPrepare();
end;

procedure TSqlStore.HandleException(Ex: Exception; SqlText: string);
var
  SB: IStringBuilder;
  Source: string;
  EventId : string;
  Text: string;
begin
  SB := TStrBuilder.Create();
  SB.AppendLine(Ex.ToString());
  SB.AppendLine();
  SB.AppendLine('Error in the following SQL statement');
  SB.AppendLine(SqlText);

  Source  := Self.ClassName + ' (' + Provider.Name + ')';
  EventId := '';
  Text    := SB.ToUtf8String();

  Logger.Error(Source, EventId, Text);
end;

function TSqlStore.Select(const SqlText: string; Params: array of const): TMemTable;
var
  Table: TMemTable;
  WasInTransaction: Boolean;
begin
  Table  := TMemTable.Create(nil);
  Result := Table;

  try
    WasInTransaction := InTransaction;
    if not WasInTransaction then
       StartTransaction();

    PrepareCommand(SqlText, Params);

    FSqlQuery.Active := True;
    FSqlQuery.First();

    DbSys.CopyDatasetStructure(FSqlQuery, Table, True, True);
    Table.CreateDataset();
    Table.Active := True;
    DbSys.CopyDataset(FSqlQuery, Table);

    UnPrepareCommand();
    if not WasInTransaction then
       Commit();
  except
    on E: Exception do
    begin
      HandleException(E, SqlText);
      Rollback();
      raise;
    end;
  end;

end;

function TSqlStore.Select(const SqlText: string): TMemTable;
begin
  Result := Select(SqlText, []);
end;

procedure TSqlStore.SelectTo(Table: TDataset; const SqlText: string; Params: array of const);
var
  WasInTransaction: Boolean;
begin

  try
    WasInTransaction := InTransaction;
    if not WasInTransaction then
       StartTransaction();

    PrepareCommand(SqlText, Params);

    FSqlQuery.Active := True;
    FSqlQuery.First();

    Table.Active := True;
    DbSys.EmptyDataset(Table);
    DbSys.CopyDataset(FSqlQuery, Table);

    UnPrepareCommand();
    if not WasInTransaction then
       Commit();
  except
    on E: Exception do
    begin
      HandleException(E, SqlText);
      Rollback();
      raise;
    end;
  end;

end;

procedure TSqlStore.SelectTo(Table: TDataset; const SqlText: string);
begin
   SelectTo(Table, SqlText, []);
end;

function TSqlStore.SelectResults(SqlText: string; const ResultFields: string; const Params: array of const): Variant;
var
  WasInTransaction: Boolean;
begin
  Result := Variants.Null;

  try
    WasInTransaction := InTransaction;
    if not WasInTransaction then
       StartTransaction();

    PrepareCommand(SqlText, Params);

    FSqlQuery.Active := True;
    FSqlQuery.First();

    if FSqlQuery.RecordCount > 0 then;
       Result := DbSys.FieldValuesToVariantArray(FSqlQuery, ResultFields);

    UnPrepareCommand();
    if not WasInTransaction then
      Commit();
  except
    on E: Exception do
    begin
      HandleException(E, SqlText);
      Rollback();
      raise;
    end;
  end;

end;

function TSqlStore.SelectResults(SqlText: string; const ResultFields: string): Variant;
begin
  Result := SelectResults(SqlText, ResultFields, []);
end;

function TSqlStore.SelectResult(SqlText: string; Default: Variant; const Params: array of const): Variant;
var
  WasInTransaction: Boolean;
begin
  Result := Default;

  try
    WasInTransaction := InTransaction;
    if not WasInTransaction then
       StartTransaction();

    PrepareCommand(SqlText, Params);

    FSqlQuery.Active := True;
    FSqlQuery.First();

    if FSqlQuery.RecordCount > 0 then;
      Result := FSqlQuery.Fields[0].Value;

    UnPrepareCommand();
    if not WasInTransaction then
       Commit();
  except
    on E: Exception do
    begin
      HandleException(E, SqlText);
      Rollback();
      raise;
    end;
  end;
end;

function TSqlStore.SelectResult(SqlText: string; Default: Variant): Variant;
begin
   Result := SelectResult(SqlText, Default, []);
end;

function TSqlStore.SelectResult(SqlText: string): Variant;
begin
  Result := SelectResult(SqlText, Variants.Null, []);
end;

function TSqlStore.IntegerResult(SqlText: string; Default: Integer; const Params: array of const): Integer;
begin
  Result := SelectResult(SqlText, Default, Params);
end;

function TSqlStore.IntegerResult(SqlText: string; Default: Integer): Integer;
begin
  Result := SelectResult(SqlText, Default, []);
end;

function TSqlStore.IntegerResult(SqlText: string): Integer;
begin
  Result := SelectResult(SqlText, -1, []);
end;

procedure TSqlStore.ExecSql(const SqlText: string; Params: array of const);
var
  WasInTransaction: Boolean;
begin

  try
    WasInTransaction := InTransaction;
    if not WasInTransaction then
       StartTransaction();

    PrepareCommand(SqlText, Params);

    FSqlQuery.ExecSQL();

    UnPrepareCommand();
    if not WasInTransaction then
       Commit();
  except
    on E: Exception do
    begin
      HandleException(E, SqlText);
      Rollback();
      raise;
    end;
  end;
end;

procedure TSqlStore.ExecSql(const SqlText: string);
begin
  ExecSql(SqlText, []);
end;

procedure TSqlStore.GetNativeSchema(SchemaName, TableName, SqlText: string; Table: TDataset);
var
  cSelects : array[0..3] of string = ( '%s where %s.ID   = -100000000'  ,
                                       '%s where %s.CODE = ''00000000''',
                                       '%s where %s.NAME = ''00000000''',
                                       '%s where 1 > 2'
                                      );
  {-------------------------------------------------}
  function TryGetFieldList(var SchemaTable: TDataset; SqlText: string): Boolean;
  begin
    Result := False;
    try
      SchemaTable := Select(SqlText);
      Result := True;
    except
    end;
  end;
  {-------------------------------------------------}

var
  SchemaTable: TDataset;
  i : Integer;
  SqlText2 : string;
begin
  if Sys.IsEmpty(TableName) then
    Sys.Error('Cannot get native schema: TableName is empty.');

  if Sys.IsEmpty(SqlText)  then
    SqlText := Format('select * from %s', [TableName]);

  if Sys.IsEmpty(SchemaName)  then
    SchemaName := TableName;

  if Table = nil then
    Sys.Error('Cannot get native schema: Table is nil.');

  SchemaTable := SchemaCache.Find(Self.ConnectionName, SchemaName);

  if SchemaTable = nil then
  begin
    for i := Low(cSelects) to High(cSelects) do
    begin
       SqlText2 := Format(cSelects[i], [SqlText, TableName]);

      if TryGetFieldList(SchemaTable, SqlText2) then
      begin
        SchemaCache.Add(Self.ConnectionName, SchemaName, SchemaTable);
        break;
      end;
    end;
  end;

  if SchemaTable = nil then
    Sys.Error('Cannot get native schema for: %s', [SchemaName]);

  DbSys.MergeStructure(SchemaTable, Table);
end;

function TSqlStore.GetConnectionName: string;
begin
  Result := FConnectionInfo.Name;
end;










{ DbSys }

class constructor DbSys.Create();
begin
end;

class destructor DbSys.Destroy();
begin
  if Assigned(FMetaDatabases) then
    FMetaDatabases.Free();
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

class function DbSys.GetMetaDatabases: TMetaDatabases; static;
begin
  if FMetaDatabases = nil then
    FMetaDatabases := TMetaDatabases.Create(True);
  Result := FMetaDatabases;
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
    if not (Sys.IsSameText(Table1.Fields[i].FieldName, Table2.Fields[i].FieldName) and (Table1.Fields[i].FieldKind = Table2.Fields[i].FieldKind)) then
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

  List   := TStringList.Create;
  try
    if Sys.IsEmpty(ResultFields) then
    begin
      for i := 0 to Dataset.Fields.Count - 1 do
        List.Add(Dataset.Fields[i].FieldName);
    end else begin
      Sys.Split(ResultFields, ';', List);
    end;

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

class procedure DbSys.AssignParams(Params: TParams; A: array of const);
var
  i : Integer;
  //Stream : TStream;
begin
  if (Params.Count > 0) and (Length(A) > 0) then
  begin

    // Just a single element.
    // Three cases:
    // 1. AssignParams(Params, [MyStream])
    // 2. AssignParams(Params, [MyDataset])
    // 3. AssignParams(Params, [VarArrayOf([Param1, ..., ParamN])])
    if Length(A) = 1 then
    begin
      case A[0].VType of
        vtObject   : if A[0].VObject is TStream then
                       AssignStreamToParam(Params[0],  A[0].VObject as TStream)
                     else if A[0].VObject is TDataset then
                       AssignParams(Params, A[0].VObject as TDataset);
        vtInterface: //if A[0].VInterface is IDictionary<string, Variant> then
                       AssignParams(Params, IDictionary<string, Variant>(A[0].VInterface));
        vtVariant  : if VarIsArray(A[0].VVariant^) then
                       AssignParams(Params, A[0].VVariant^);
      end;
    end else begin
      // array with more than a single element
      for i := 0 to High(A) do
      begin
        case A[i].VType of
          vtInteger       : Params.Items[i].Value := A[i].VInteger;
          vtBoolean       : Params.Items[i].Value := A[i].VBoolean;
          vtChar          : Params.Items[i].Value := A[i].VChar;
          vtWideChar      : ;
          vtExtended      : Params.Items[i].Value := A[i].VExtended^;
          vtString        : Params.Items[i].Value := A[i].VString^;    // short string
          vtPointer       : ;                                          // Longint(Args[i].VPointer)
          vtPChar         : ;
          vtObject        : if A[i].VObject is TStream then
                              AssignStreamToParam(Params[i],  A[i].VObject as TStream);
          vtClass         : ;                                          // Args[i].VClass
          vtPWideChar     : ;
          vtAnsiString    : Params.Items[i].Value := AnsiString(A[i].VAnsiString);
          vtCurrency      : Params.Items[i].Value := A[i].VCurrency^;
          vtVariant       : Params.Items[i].Value := A[i].VVariant^;
          vtInterface     : ;
          vtWideString    : Params.Items[i].Value := WideString(A[i].VWideString);
          vtInt64         : Params.Items[i].Value := A[i].VInt64^;
          vtUnicodeString : Params.Items[i].Value := UnicodeString(A[i].VUnicodeString);
          vtQWord         : Params.Items[i].Value := A[i].VQWord^;
        end;
      end;
    end;
  end;

end;

class procedure DbSys.AssignParams(Params: TParams; tblParams: TDataset);
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


{ to be used as AssignParams(Params, VarArrayOf([Param1, ..., ParamN])); }
class procedure DbSys.AssignParams(Params: TParams; A: array of Variant);
var
  i : Integer;
  Stream: TStream;
begin
  if (Length(A) > 0) and (Params.Count > 0) then
  begin
    for i := Low(A) to High(A) do
    begin
      if Sys.IsBox(A[i]) and (Sys.UnBox(A[i]) is TStream) then
      begin
        Stream :=  TStream(Sys.UnBox(A[i]));
        Params.Items[i].DataType := ftBlob;
        Params.Items[i].Size     := Stream.Size;
        Stream.Position := 0;
        Params.Items[i].LoadFromStream(Stream, ftBlob);
      end else begin
        Params.Items[i].Value := A[i];
      end;
    end;
  end;

end;

class procedure DbSys.AssignParams(Params: TParams; const ParamsDic: IDictionary<string, Variant>);
var
  Entry: TGenKeyValue<string, Variant>;
begin
  for Entry in ParamsDic do
    AssignParam(Params, Entry.Key, Entry.Value);
end;

class procedure DbSys.AssignStreamToParam(Param: TParam; Stream: TStream);
begin
  Param.DataType  := ftBlob;
  Param.Size      := Stream.Size;
  Stream.Position := 0;
  Param.LoadFromStream(Stream, ftBlob);
end;

class procedure DbSys.AssignParam(Params: TParams; ParamName: string; Value: Variant);
var
  Param: TParam;
begin
  Param := Params.FindParam(ParamName);
  if Assigned(Param) then
    Param.Value := Value;
end;


{ Sql }

class function Sql.IsMasked(Value: string): Boolean;
begin
  if Sys.IsEmpty(Value) then
     Exit(False);

  if Value.Contains('*') or Value.Contains('?') or  Value.Contains('%') then
    Result := True
  else
    Result := False;
end;















initialization
{$I metadatasqls.lrs}

end.

