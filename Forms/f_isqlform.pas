unit f_ISqlForm;

{$mode DELPHI}{$H+}
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
  , LCLType

  , Tripous
  , Tripous.Data
  , Tripous.Logs

  ,f_BaseForm
  ;

type

  TSqlHistory = class;

  { TISqlForm }
  TISqlForm = class(TBaseForm)
    btnExec: TToolButton;
    btnExit: TToolButton;
    btnNext: TToolButton;
    btnPrior: TToolButton;
    btnShowIdColumns: TToolButton;
    DS: TDataSource;
    edtSql: TSynEdit;
    ImageListISql: TImageList;
    pagerGrids: TPageControl;
    Splitter1: TSplitter;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
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
    FInitialSql: string;
    FPage: TTabSheet;

    FMetaDatabase: TMetaDatabase;
    FSqlHistory: TSqlHistory;
    FIdColumnsVisible : Boolean;

    procedure SqlHistory_CurrentSqlTextChanged(Sender: TObject);
    procedure ToggleShowIdColumns();
    procedure ExecSql();

    procedure DoExecute(Param: TObject);
    procedure DoCompleted(Data: PtrInt);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure AnyClick(Sender: TObject); override;
    procedure EnableCommands(); override;
    procedure FormInitialize(); override;
  public
    constructor Create(Owner: TComponent; InitInfo: TFormInitInfo); overload; override;
    destructor Destroy(); override;

    class function CreateAsChild(Pager: TPageControl; MetaDatabase: TMetaDatabase; InitialSql: string = ''): TISqlForm;

    property Page: TTabSheet  read FPage;
    property InitialSql: string   read FInitialSql;
    property Id: Integer read GetFormId;
    property MetaDatabase: TMetaDatabase  read FMetaDatabase;
  end;


  { TSqlStatementItem }
  { Represents an Sql statement executed by the ISql form, for history (Prior-Next) purposes }
  TSqlStatementItem = class
  private
    FSqlText: string;
    FStatementName: string;
    function GetIsSelect: Boolean;
  public
    constructor Create(SqlText: string);

    property SqlText: string read FSqlText;
    property IsSelect: Boolean read GetIsSelect;
    property StatementName: string read FStatementName;
  end;

  { TSqlHistoryItem }
  { Represents a history item. A history item is created every time the user
    executes Sql in the ISqlForm. Since the form Sql editor may contain more than
    a single statement, the history item parses the editor text into SqlStatementItem items.  }
  TSqlHistoryItem = class
  private
    FSqlStatements: IList<TSqlStatementItem>;
    FSqlText: string;
  public
    constructor Create(SqlText: string);

    property SqlText: string read FSqlText;
    property SqlStatements: IList<TSqlStatementItem> read FSqlStatements;
  end;

  { TSqlHistory }
  { Controls the history items. }
  TSqlHistory = class
  private
    FItems: IList<TSqlHistoryItem>;
    FCurrentSqlText: string;
    FIndex: Integer;
    FCurrentSqlTextChanged: TNotifyEvent;
    function GetBof: Boolean;
    function GetCurrent: TSqlHistoryItem;
    function GetEof: Boolean;
    function GetCurrentSqlText: string;
    procedure SetCurrentSqlText(AValue: string);
  public
    constructor Create();

    {Called every time the user executes Sql statements.
    SqlText is the text of the Sql editor of the ISqlForm.
    The specified SqlText is used to create a HistoryItem which parses  the Text into SqlStatementItem items.}
    procedure Add(SqlText: string);
    { Advances an internal index into the next HistoryItem, if any, and assigns the CurrentSqlText property according to that HistoryItem. }
    procedure Next();
    { Sets an internal index to the prior HistoryItem, if any, and assigns the CurrentSqlText property according to that HistoryItem.}
    procedure Prior();

    { Returns true if the internal index is in the last HistoryItem   }
    property Eof: Boolean read GetEof;
    { Returns true if the internal index is in the first HistoryItem  }
    property Bof: Boolean read GetBof;
    { Gets the current HistoryItem, if any, else nil     }
    property Current: TSqlHistoryItem read GetCurrent;
    { Ges the Text of the Current HistoryItem, if any, else string.Empty    }
    property CurrentSqlText: string read GetCurrentSqlText;

    property CurrentSqlTextChanged: TNotifyEvent read FCurrentSqlTextChanged write FCurrentSqlTextChanged;
  end;

implementation

{$R *.lfm}



var
  StatementNames : array of string = ['select', 'execute', 'exec', 'insert', 'update', 'delete', 'create', 'alter', 'drop', 'truncate'];

{ TSqlStatementItem }
function TSqlStatementItem.GetIsSelect: Boolean;
begin
  Result := FStatementName = 'select';
end;

constructor TSqlStatementItem.Create(SqlText: string);
var
  sSqlText, Item: string;
begin
  inherited Create();
  FSqlText := SqlText;
  sSqlText := SqlText.Trim();
  for Item in StatementNames do
    if sSqlText.StartsWith(Item, True) then
    begin
      FStatementName := Item;
      break;
    end;
end;

{ TSqlHistoryItem }

constructor TSqlHistoryItem.Create(SqlText: string);
var
  SB: IStringBuilder;
  Lines: TStringArray;
  Line: string;

  TextList : IList<string>;
  SqlList  : IList<string>;
  LastLine : string;
  //S        : string;
  S2       : string;
  i        : Integer;


  StatementName: string;

  // -----------------------------------------------------------------------
  function GetStatementName(sLine: string): string;
  var
    sName: string;
  begin
    Result := '';

    sLine := sLine.Trim();
    if not Sys.IsEmpty(sLine) then
    begin
      for sName in StatementNames do
      begin
        if sLine.StartsWith(sName, True) then
        begin
          if sName = 'select' then
          begin
            if not ((Length(LastLine) > 0) and (LastLine[LastLine.Length - 1] = '('))  then
              Result := sName;
            break;
          end else begin
            Result := sName;
            break;
          end;
        end;
      end;
    end;
  end;
  // -----------------------------------------------------------------------
  procedure AddText(List: IList<string>; Text: string; StripEmptyLines: Boolean);
  var
    SplitOptions: TStringSplitOptions;
    Lines2: TStringArray;
    Item: string;
  begin
    if not Sys.IsEmpty(Text) then
    begin
      if StripEmptyLines then
        SplitOptions := TStringSplitOptions.ExcludeEmpty
      else
        SplitOptions := TStringSplitOptions.None;

        Lines2 := Text.Split([sLineBreak], SplitOptions);
        for Item in Lines2 do
        begin
          if not (Item.Trim().StartsWith('--')
               or Item.Trim().StartsWith('##')
               or Item.Trim().StartsWith('//')) then
          List.Add(Item);
        end;
    end;
  end;
  // -----------------------------------------------------------------------
begin
  inherited Create();
  FSqlText := SqlText;

  FSqlStatements := TGenObjectList<TSqlStatementItem>.Create(True, False);

  // strip empty lines
  SB := TStrBuilder.Create();
  Lines := SqlText.Trim().Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);

  for Line in Lines do
  begin
    if not Sys.IsEmpty(Line) then
       SB.AppendLine(Line);
  end;

  SqlText := SB.ToString();

  // strip comments from Text
  TextList := TGenList<string>.Create();
  SqlList  := TGenList<string>.Create();
  LastLine := '';

  AddText(TextList, SqlText, True);

  for i := 0 to TextList.Count - 1 do
  begin
    Line := TextList[i].Trim();

    if not Sys.IsEmpty(Line) then
    begin
      StatementName := GetStatementName(Line);
      if not Sys.IsEmpty(StatementName) then
      begin
        S2 := Sys.ToText(SqlList);
        if not Sys.IsEmpty(S2) then
           SqlStatements.Add(TSqlStatementItem.Create(S2));
        SqlList.Clear();
      end;

      LastLine := Line;
      SqlList.Add(TextList[i]);
    end;

    {
    if not Sys.IsEmpty(S) then
    begin
      if (S.StartsWith('select', True) and not ((Length(LastLine) > 0) and (LastLine[LastLine.Length - 1] = '(')))
         or S.StartsWith('execute', True)
         or S.StartsWith('exec', True)
         or S.StartsWith('insert', True)
         or S.StartsWith('update', True)
         or S.StartsWith('delete', True)
         or S.StartsWith('create', True)
         or S.StartsWith('alter', True)
         or S.StartsWith('drop', True)
         or S.StartsWith('truncate', True) then
      begin
        S2 := Sys.ToText(SqlList);
        if not Sys.IsEmpty(S2) then
           SqlStatements.Add(TSqlStatementItem.Create(S2));
        SqlList.Clear();
      end;

      LastLine := S;
      SqlList.Add(TextList[i]);
    end;
    }
  end;

  S2 := Sys.ToText(SqlList);
  if not Sys.IsEmpty(S2) then
     SqlStatements.Add(TSqlStatementItem.Create(S2));

end;


{ TSqlHistory }
constructor TSqlHistory.Create();
begin
  inherited Create();
  FItems := TGenObjectList<TSqlHistoryItem>.Create(True, False);
end;

function TSqlHistory.GetEof: Boolean;
begin
  if FItems.Count = 0 then
     Result := True
  else
    Result := FIndex = FItems.Count - 1;
end;

function TSqlHistory.GetBof: Boolean;
begin
  if FItems.Count = 0 then
    Result := True
  else
    Result := FIndex = 0;
end;

function TSqlHistory.GetCurrent: TSqlHistoryItem;
begin
  Result := nil;
  if (FIndex >= 0) and (FIndex <= FItems.Count - 1) then
    Result := FItems[FIndex];
end;

function TSqlHistory.GetCurrentSqlText: string;
begin
  if Sys.IsEmpty(FCurrentSqlText) then
    Result := ''
  else
    Result := FCurrentSqlText;
end;

procedure TSqlHistory.SetCurrentSqlText(AValue: string);
begin
  if AValue <> FCurrentSqlText then
  begin
    FCurrentSqlText := AValue;
    if Assigned(CurrentSqlTextChanged) then
       CurrentSqlTextChanged(Self);
  end;
end;

procedure TSqlHistory.Next();
begin
  if not (FIndex + 1 > FItems.Count - 1) then
  begin
    Inc(FIndex);
    SetCurrentSqlText(FItems[FIndex].SqlText);
  end else begin
    SetCurrentSqlText('');
  end;
end;

procedure TSqlHistory.Prior();
begin
  if not (FIndex - 1 < 0) then
  begin
    Dec(FIndex);
    SetCurrentSqlText(FItems[FIndex].SqlText);
  end;
end;

procedure TSqlHistory.Add(SqlText: string);
begin
  if not Sys.IsEmpty(SqlText) then
  begin
    if not ((FIndex < 0) or (FIndex > FItems.Count - 1)) then
    begin
      if not Sys.IsSameText(SqlText.Trim(), FItems[FIndex].SqlText.Trim()) then
      begin
        FIndex := FItems.Count;
        FItems.Add(TSqlHistoryItem.Create(SqlText));
      end;
    end else begin
      FIndex := FItems.Count;
      FItems.Add(TSqlHistoryItem.Create(SqlText));
    end;

  end;

end;


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

    procedure SetIdColumnsVisible(Value: Boolean);
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
    Grid.AutoAdjustColumns();
  finally
    Grid.EndUpdate();
  end;

end;

destructor TGridPage.Destroy();
begin
  Table.Free();
  inherited Destroy();
end;

procedure TGridPage.SetIdColumnsVisible(Value: Boolean);
var
  i : Integer;
  Column: TColumn;
begin
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    Column := Grid.Columns[i];
    if Column.FieldName.EndsWith('Id', True) then
       Column.Visible := Value;
  end;
end;



{ TISqlForm }

class function TISqlForm.CreateAsChild(Pager: TPageControl; MetaDatabase: TMetaDatabase; InitialSql: string): TISqlForm;
var
  InitInfo: TFormInitInfo;
  Page : TTabSheet;
begin
  Page  := Pager.AddTabSheet();

  InitInfo := TFormInitInfo.Create();
  InitInfo.FormMode := fmChild;
  InitInfo.Parent   := Page;
  InitInfo.Caption  := MetaDatabase.DisplayText;
  InitInfo.Tag      := MetaDatabase;
  InitInfo.vTag     := InitialSql;

  Result := TISqlForm.Create(Page, InitInfo);
  Result.Visible := True;
  Pager.ActivePage := Page;
end;

constructor TISqlForm.Create(Owner: TComponent; InitInfo: TFormInitInfo);
begin
  inherited Create(Owner, InitInfo);

  FPage := FInitInfo.Parent as TTabSheet;
  FMetaDatabase := FInitInfo.Tag as TMetaDatabase;
  FInitialSql := FInitInfo.vTag;
  Caption := IntToStr(Id) + '. ' + FInitInfo.Caption;

  FSqlHistory := TSqlHistory.Create();
  FSqlHistory.CurrentSqlTextChanged := SqlHistory_CurrentSqlTextChanged;
end;

destructor TISqlForm.Destroy();
begin
  FSqlHistory.Free();
  inherited Destroy();
end;

procedure TISqlForm.FormInitialize();
begin
  inherited FormInitialize();

  btnExit.OnClick := AnyClick;
  btnPrior.OnClick := AnyClick;
  btnNext.OnClick := AnyClick;
  btnExec.OnClick := AnyClick;
  btnShowIdColumns.OnClick := AnyClick;

  EnableCommands();

  edtSql.Clear();

  FIdColumnsVisible := False;
  btnShowIdColumns.Down := not FIdColumnsVisible;

  //ToggleShowIdColumns();
  //Application.ProcessMessages();

  if not Sys.IsEmpty(InitialSql) then
  begin
    edtSql.Text := InitialSql;
    btnExec.Click();
  end;

end;

procedure TISqlForm.AnyClick(Sender: TObject);
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

procedure TISqlForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and (Shift = []) then
    btnExec.Click();

  inherited KeyDown(Key, Shift);
end;

procedure TISqlForm.SqlHistory_CurrentSqlTextChanged(Sender: TObject);
begin
  edtSql.Text := FSqlHistory.CurrentSqlText;
end;

procedure TISqlForm.ToggleShowIdColumns();
var
  i : Integer;
  Page: TGridPage;
begin
  FIdColumnsVisible := not FIdColumnsVisible;
  btnShowIdColumns.Down := not FIdColumnsVisible;

  for i := 0 to pagerGrids.PageCount - 1 do
  begin
    Page := pagerGrids.Pages[i] as TGridPage;
    Page.SetIdColumnsVisible(FIdColumnsVisible);
  end;

end;

procedure TISqlForm.EnableCommands();
begin
  btnPrior.Enabled := not FSqlHistory.Bof;
  btnNext.Enabled  := not FSqlHistory.Eof;
end;

procedure TISqlForm.ExecSql();
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

procedure TISqlForm.DoExecute(Param: TObject);
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

procedure TISqlForm.DoCompleted(Data: PtrInt);
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

      GridPage.SetIdColumnsVisible(FIdColumnsVisible);
      //Application.ProcessMessages();
    end;
  finally
    Info.Free();
  end;

end;









end.

