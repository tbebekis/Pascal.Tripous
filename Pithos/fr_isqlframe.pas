unit fr_ISqlFrame;

{$MODE DELPHI}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
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
  ,o_App

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
  private class var
    FStatementCounter: Integer;
    FSelectCounter: Integer;
  private type
    TSqlExecInfo = class
      SqlText: string;
      StatementName: string;
      IsSelect: Boolean;
      StatementCounter: Integer;
      SelectCounter: Integer;
      Table: TDataset;
      ErrorText: string;
    end;
  private
    FId: Integer;

    FInitialSql: string;
    FMetaDatabase: TMetaDatabase;
    FPage: TTabSheet;
    FSqlHistory: TSqlHistory;

    procedure AnyClick(Sender: TObject);
    procedure SqlHistory_CurrentSqlTextChanged(Sender: TObject);
    procedure ToggleShowIdColumns();
    procedure EnableCommands();
    procedure ExecSql();

    procedure DoExecute(Param: TObject);
    procedure DoCompleted(Data: PtrInt);
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
  Grid.BeginUpdate();
  try
    Grid.DataSource := DS;
  finally
    Grid.EndUpdate();
  end;

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

  EnableCommands();

  if not Sys.IsEmpty(InitialSql) then
  begin
    edtSql.Text := InitialSql;
    btnExec.Click();
  end;

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
  SqlExecInfo  : TSqlExecInfo;
begin
  pagerGrids.Clear();
  Application.ProcessMessages();

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

        SqlExecInfo := TSqlExecInfo.Create();
        SqlExecInfo.SqlText := SqlStatementItem.SqlText;
        SqlExecInfo.StatementName := SqlStatementItem.StatementName;
        SqlExecInfo.IsSelect := SqlStatementItem.IsSelect;
        SqlExecInfo.StatementCounter := FStatementCounter;
        SqlExecInfo.SelectCounter := FSelectCounter;
        SqlExecInfo.Table := nil;
        SqlExecInfo.ErrorText :='';

        TAsync.Run(SqlExecInfo, DoExecute);

        Application.ProcessMessages();


      end;
    end;
  end;

end;

procedure TISqlFrame.DoExecute(Param: TObject);
var
  SqlStore     : TSqlStore;
  Info         : TSqlExecInfo;
begin
  Info := TSqlExecInfo(Param);
  try
    SqlStore := TSqlStore.Create(MetaDatabase.SqlStore.ConnectionInfo);
    try
      if Info.IsSelect then
         Info.Table := MetaDatabase.SqlStore.Select(Info.SqlText)
      else
         MetaDatabase.SqlStore.ExecSql(Info.SqlText);

      // Application.QueueAsyncCall
      // https://wiki.lazarus.freepascal.org/Asynchronous_Calls
      // https://forum.lazarus.freepascal.org/index.php?topic=47603.0
      Application.QueueAsyncCall(DoCompleted, PtrInt(Info));
    finally
      SqlStore.Free();
    end;
  except
    on E: Exception do
    begin
      Info.ErrorText := E.ToString();
    end;
  end;
end;

procedure TISqlFrame.DoCompleted(Data: PtrInt);
var
  GridPage     : TGridPage;
  SB           : IStringBuilder;
  Info         : TSqlExecInfo ;
begin
  Info := TSqlExecInfo(Data);
  try
    if not Sys.IsEmpty(Info.ErrorText) then
    begin
      LogBox.AppendLine('ERROR: ' + Info.ErrorText);
      LogBox.AppendLine('in the following statement: ');
      LogBox.AppendLine(Info.SqlText);
    end else begin
      if Info.IsSelect then
      begin
        GridPage := TGridPage.CreatePage(pagerGrids, Info.Table);
        GridPage.Caption := IntToStr(Info.SelectCounter);
        //Application.ProcessMessages();
      end;

      SB := TStrBuilder.Create();
      SB.Append(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now));
      SB.Append('[' + IntToStr(Info.StatementCounter) + '] ');
      SB.Append(Info.StatementName + ' is executed.');
      if Assigned(Info.Table) then
         SB.Append(' Rows: ' + IntToStr(Info.Table.RecordCount));
      SB.AppendLine();
      SB.AppendLine(Info.SqlText);

      LogBox.AppendLine(SB.ToString());
      //Application.ProcessMessages();
    end;
  finally
    Info.Free();
  end;



end;

end.

