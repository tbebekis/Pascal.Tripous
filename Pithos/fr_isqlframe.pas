unit fr_ISqlFrame;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , DB
  , Forms
  , Controls
  , ComCtrls
  , ExtCtrls
  , DBGrids
  , SynEdit
  , SynHighlighterSQL

  , Tripous
  , Tripous.Data
  , Tripous.Logs
  //,o_App

  ,o_SqlHistory
  ;

type

  { TISqlFrame }

  TISqlFrame = class(TFrame)
    DS: TDataSource;
    edtSql: TSynEdit;
    ImageListISql: TImageList;
    pagerGrids: TPageControl;
    Splitter1: TSplitter;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar: TToolBar;
    btnExit: TToolButton;
    btnPrior: TToolButton;
    btnNext: TToolButton;
    btnExec: TToolButton;
    btnShowIdColumns: TToolButton;
  private
    FId: Integer;

    FInitialSql: string;
    FMetaDatabase: TMetaDatabase;
    FPage: TTabSheet;
    FSqlHistory: TSqlHistory;

    FStatementCounter: Integer;
    FSelectCounter: Integer;

    procedure AnyClick(Sender: TObject);
    procedure SqlHistory_CurrentSqlTextChanged(Sender: TObject);
    procedure ToggleShowIdColumns();
    procedure EnableCommands();
    procedure ExecSql();

    { executed inside a secondary thread }
    procedure OnSafeAsyncExecute(const Info: IInterface);
    { execute inside the main thread }
    procedure OnSafeAsyncCompleted(const Info: IInterface);
  public
    constructor Create(Page: TTabSheet; MetaDatabase: TMetaDatabase; InitialSql: string); overload;
    destructor Destroy(); override;

    property MetaDatabase: TMetaDatabase  read FMetaDatabase;
    property Page: TTabSheet  read FPage;
    property InitialSql: string   read FInitialSql;
    property Id: Integer read FId;
  end;

implementation

{$R *.lfm}

uses
  o_App
  ;


type
  { TGridPage }
  TGridPage = class(TTabSheet)
  private
    Table: TDataset;
    DS: TDatasource;
    Grid: TDBGrid;
  public
    constructor CreatePage(AParent: TPageControl; Table: TDataset);
    destructor Destroy(); override;
  end;

{ TGridPage }

constructor TGridPage.CreatePage(AParent: TPageControl; Table: TDataset);
begin
  inherited Create(AParent);
  Self.Table := Table;
  DS := TDataSource.Create(Self);
  Grid := TDBGrid.Create(Self);
  PageControl := AParent;
  Grid.Parent := Self;
  Grid.Align := alClient;
  Grid.ReadOnly := True;
  DS.DataSet := Table;
  Grid.DataSource := DS;
end;

destructor TGridPage.Destroy();
begin
  Table.Free();
  inherited Destroy();
end;



{ TISqlFrame }

constructor TISqlFrame.Create(Page: TTabSheet; MetaDatabase: TMetaDatabase; InitialSql: string);
begin
  inherited Create(Page);
  FPage := Page;
  FMetaDatabase := MetaDatabase;
  FInitialSql := InitialSql;
  Parent := Page;
  Align := alClient;
  FId := App.NextSqlPageId();

  FSqlHistory := TSqlHistory.Create();
  FSqlHistory.CurrentSqlTextChanged := SqlHistory_CurrentSqlTextChanged;

  Page.Caption := IntToStr(Id) + '. ' + MetaDatabase.DisplayText;

  btnExit.OnClick := AnyClick;
  btnPrior.OnClick := AnyClick;
  btnNext.OnClick := AnyClick;
  btnExec.OnClick := AnyClick;
  btnShowIdColumns.OnClick := AnyClick;

  if not Sys.IsEmpty(InitialSql) then
    edtSql.Text := InitialSql;

  EnableCommands();
end;

destructor TISqlFrame.Destroy();
begin

  inherited Destroy();
end;

procedure TISqlFrame.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then
  begin
    FPage.Free();
  end else begin
         if btnPrior = Sender then FSqlHistory.Prior()
    else if btnNext = Sender then FSqlHistory.Next()
    else if btnShowIdColumns  = Sender then ToggleShowIdColumns()
    else if btnExec = Sender then ExecSql()
    ;
    EnableCommands();
  end;
end;

procedure TISqlFrame.SqlHistory_CurrentSqlTextChanged(Sender: TObject);
begin
  edtSql.Text := FSqlHistory.CurrentSqlText;
end;

procedure TISqlFrame.ToggleShowIdColumns();
begin
  btnShowIdColumns.Down := not btnShowIdColumns.Down;
  // TODO: ToggleShowIdColumns();
end;

procedure TISqlFrame.EnableCommands();
begin
  btnPrior.Enabled := not FSqlHistory.Bof;
  btnNext.Enabled  := not FSqlHistory.Eof;
end;

procedure TISqlFrame.ExecSql();
var
  SqlText: string;
  SqlHistoryItem: TSqlHistoryItem;
  SqlStatementItem : TSqlStatementItem;
  SqlExecInfo  : ISqlExecInfo;
begin
  pagerGrids.Clear();

  SqlText := '';
  if not Sys.IsEmpty(edtSql.SelText) then
    SqlText := edtSql.SelText
  else
    SqlText := edtSql.Text;

  if Length(SqlText.Trim()) > 0 then
  begin
    FSqlHistory.Add(SqlText);

    SqlHistoryItem := FSqlHistory.Current;

    if Assigned(SqlHistoryItem) then
    begin

      for SqlStatementItem in SqlHistoryItem.SqlStatements do
      begin
        Inc(FStatementCounter);

        if SqlStatementItem.IsSelect then
           Inc(FSelectCounter);

        SqlExecInfo  := TSqlExecInfo.Create(SqlStatementItem.SqlText, SqlStatementItem.IsSelect, FStatementCounter, FSelectCounter);

        Sys.SafeAsyncExecute(SqlExecInfo, OnSafeAsyncExecute, OnSafeAsyncCompleted);
      end;
    end;
  end;

end;

procedure TISqlFrame.OnSafeAsyncExecute(const Info: IInterface);
var
  SqlExecInfo  : ISqlExecInfo;
begin
  SqlExecInfo := Info as ISqlExecInfo;
  try
    if SqlExecInfo.IsSelect then
       SqlExecInfo.Table := MetaDatabase.SqlStore.Select(SqlExecInfo.SqlText)
    else
       MetaDatabase.SqlStore.ExecSql(SqlExecInfo.SqlText);
  except
    on E: Exception do
    begin
      SqlExecInfo.ErrorText := E.ToString();
    end;
  end;
end;

procedure TISqlFrame.OnSafeAsyncCompleted(const Info: IInterface);
var
  SqlExecInfo  : ISqlExecInfo;
  GridPage: TGridPage;
begin
  SqlExecInfo := Info as ISqlExecInfo;
  if not Sys.IsEmpty(SqlExecInfo.ErrorText) then
  begin
    LogBox.AppendLine('ERROR: ' + SqlExecInfo.ErrorText);
    LogBox.AppendLine('in the following statement: ');
    LogBox.AppendLine(SqlExecInfo.SqlText);
  end else begin
    if SqlExecInfo.IsSelect then
    begin
      GridPage := TGridPage.CreatePage(pagerGrids, SqlExecInfo.Table);
      GridPage.Caption := IntToStr(SqlExecInfo.SelectCounter);
    end;
  end;
end;

end.

