unit frm_MainForm;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DBGrids, ComCtrls, Variants, TypInfo, Contnrs, DB, BufDataset

  ,Tripous.FilterParser
  ;

type
  TBufTable = class;

  { TMainForm }
  TMainForm = class(TForm)
    btnExecuteParser: TButton;
    btnExecuteScanner: TButton;
    btnClearLog: TButton;
    cboFilter: TComboBox;
    gridVariables: TDBGrid;
    Label1: TLabel;
    mmoLog: TMemo;
    Pager: TPageControl;
    Panel1: TPanel;
    tabLog: TTabSheet;
    tabVariables: TTabSheet;
  private
    tblVariables : TBufTable;
    dsVariables  : TDatasource;

    procedure AnyClick(Sender: TObject);
    procedure ExecuteScanner();
    procedure ExecuteParser();

    procedure OnFilterError(Sender: TObject; ErrorMessage: string);
    procedure OnFilterVariableValue(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
  protected
    procedure DoShow; override;

    class procedure AdjustGridColumns(Grid: TDBGrid);
  end;

  { TBufTable }
  TBufTable = class(TBufDataset)
  protected
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

class procedure TMainForm.AdjustGridColumns(Grid: TDBGrid);
var
  i : Integer;
begin
  for i := 0 to Grid.Columns.Count-1 do
    Grid.Columns[i].Width := 100;
end;

procedure TMainForm.DoShow;
begin
  inherited DoShow;

  tblVariables := TBufTable.Create(Self);
  tblVariables.FieldDefs.Add('Name', ftString, 32);
  tblVariables.FieldDefs.Add('Value', ftString, 128);
  tblVariables.CreateDataset();
  tblVariables.Active := True;

  tblVariables.Append();
  tblVariables.FieldByName('Name').AsString := 'Num';
  tblVariables.FieldByName('Value').AsString := '3';
  tblVariables.Post();

  tblVariables.Append();
  tblVariables.FieldByName('Name').AsString := 'Name';
  tblVariables.FieldByName('Value').AsString := 'Pascal';
  tblVariables.Post();

  dsVariables := TDataSource.Create(Self);
  dsVariables.DataSet := tblVariables;

  gridVariables.DataSource := dsVariables;
  AdjustGridColumns(gridVariables);

  cboFilter.Clear();
  cboFilter.Items.Add('Num in (1, 4, 3, 8) ');
  cboFilter.Items.Add('Name like ''%sc%'' ');
  cboFilter.Items.Add('1 > 2');
  cboFilter.Items.Add('2 * 5 + 2');
  cboFilter.Items.Add('2 * 5 + 2 >= 9');
  cboFilter.ItemIndex := 0;

  btnExecuteParser.OnClick := @AnyClick;
  btnExecuteScanner.OnClick := @AnyClick;
  btnClearLog.OnClick := @AnyClick;

  Pager.ActivePage := tabLog;
  mmoLog.Clear();
end;

procedure TMainForm.AnyClick(Sender: TObject);
begin
  if btnExecuteParser = Sender then
    ExecuteParser()
  else if btnExecuteScanner = Sender then
    ExecuteScanner()
  else if btnClearLog = Sender then
    mmoLog.Clear()
  ;
end;

procedure TMainForm.ExecuteScanner();
var
  Scanner : TFilterScanner;
  TokenList: TObjectList;
  i : Integer;
  Source: string;
begin
  Pager.ActivePage := tabLog;

  Scanner := TFilterScanner.Create();
  try
    Source := cboFilter.Text;
    TokenList := Scanner.Scan(Source);

    for i := 0 to TokenList.Count - 1 do
        mmoLog.Lines.Append(TToken(TokenList[i]).ToString());
  finally
    Scanner.Free;
  end;
end;

procedure TMainForm.ExecuteParser();
var
  Parser : TFilterParser;
  Source: string;
  V: Variant;
begin
  Pager.ActivePage := tabLog;

  Parser := TFilterParser.Create();
  try
    Source := cboFilter.Text;
    Parser.Parse(Source);

    mmoLog.Lines.Add(Parser.ToString());

    Parser.OnVariable := @OnFilterVariableValue;

    V := Parser.Evaluate();
    mmoLog.Lines.Append(VarToStr(V));
  finally
    Parser.Free;
  end;
end;

procedure TMainForm.OnFilterError(Sender: TObject; ErrorMessage: string);
begin
  mmoLog.Lines.Add(ErrorMessage);
end;

procedure TMainForm.OnFilterVariableValue(Sender: TObject; Variable: string; ClientTag: Pointer; var Value: Variant);
begin
  if tblVariables.Locate('Name', Variable, [loCaseInsensitive]) then
  begin
    Value := tblVariables.FieldByName('Value').AsString;
    mmoLog.Lines.Append(Format('Variable: %s = %s', [Variable, Value]));
  end else begin
    mmoLog.Lines.Append(Format('Variable not found: %s', [Variable]));
  end;

end;

{ TBufTable }
procedure TBufTable.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
   { nothing - this method is abstract in the ancestor, for some reason }
end;





end.

