unit frm_FilterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , StdCtrls
  , Graphics
  , Dialogs
  , DB
  , DBCtrls
  , DBGrids

  ,TypInfo

  ,O_App
  ,Tripous.MemTable
  ;

type
  TFilterState = (fsNone, fsFilter, fsEvent);
  TFilterStates = set of TFilterState;

type

  { TFilterForm }

  TFilterForm = class(TForm)
    btnApplyFilter: TButton;
    btnUseFilterEvent: TButton;
    btnCancelFilterEvent: TButton;
    btnCancelFilter: TButton;
    edtAmount: TEdit;
    edtFilter: TEdit;
    Grid: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
  private
    FFilterState: TFilterState;
    procedure SetFilterState(Value: TFilterState);
  private
    DS: TDatasource;
    Table: TMemTable;
    FAmount : Double;

    procedure InitializeTest();
    procedure AnyClick(Sender: TObject);

    procedure OnFilterRecord(DataSet: TDataSet; var Accept: Boolean);

    property FilterState : TFilterState read FFilterState write SetFilterState;

  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}



procedure TFilterForm.SetFilterState(Value: TFilterState);
begin
  FFilterState := Value;

  btnApplyFilter.Enabled       := FFilterState in [fsNone];
  btnCancelFilter.Enabled      := FFilterState in [fsFilter];
  btnUseFilterEvent.Enabled    := FFilterState in [fsNone];
  btnCancelFilterEvent.Enabled := FFilterState in [fsEvent];
end;

procedure TFilterForm.InitializeTest();
const
    Names  : array[0..6] of string = ('Teo', 'George', 'John', 'Mike', 'Nik', 'Peter', 'Bill');
    //Days : array[0..6] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    Countries : array[0..6] of string = ('Greece', 'Spain', 'Italy', 'France', 'Germany', 'Denmark', 'Poland');
var
  i: Integer;
begin
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Name', ftString, 100);
  Table.FieldDefs.Add('Country', ftString, 100);
  Table.FieldDefs.Add('Amount', ftFloat);
  Table.FieldDefs.Add('Date', ftDate);

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

  Table.Active := True;

  App.AdjustGridColumns(Grid);

  Randomize();
  for i := 0 to 9 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := Names[Random(Length(Names) - 1)];          // range 0..6
    Table.FieldByName('Country').AsString := Countries[Random(Length(Countries) - 1)];
    Table.FieldByName('Amount').AsFloat := (Random(99999) + 1) * 1.5;
    Table.FieldByName('Date').AsDateTime := SysUtils.Date() + Random(7);
    Table.Post();
  end;

  btnApplyFilter.OnClick := @AnyClick;
  btnCancelFilter.OnClick := @AnyClick;
  btnUseFilterEvent.OnClick := @AnyClick;
  btnCancelFilterEvent.OnClick := @AnyClick;

  Table.MoveBy(3);
  edtFilter.Text := 'Name = ' + QuotedStr(Table.FieldByName('Name').AsString);
  Table.First;

  FilterState := fsNone;
end;

procedure TFilterForm.AnyClick(Sender: TObject);

begin
  if (btnApplyFilter = Sender) then
  begin
    if Length(edtFilter.Text) > 0 then
    begin
      FilterState := fsFilter;

      Table.Filtered := False;
      Table.Filter := edtFilter.Text;
      Table.Filtered := True;
    end;
  end else if (btnCancelFilter = Sender) then
  begin
    FilterState := fsNone;
    Table.Filtered := False;
  end else if (btnUseFilterEvent = Sender) then
  begin
    FilterState := fsEvent;

    Table.Filter := '';

    if not TryStrToFloat(edtAmount.Text, FAmount) then
       FAmount := 50000;

    Table.OnFilterRecord := @OnFilterRecord;
    Table.Filtered := True;
  end else if (btnCancelFilterEvent = Sender) then
  begin
    FilterState := fsNone;

    Table.OnFilterRecord := nil;
    Table.Filtered := False;
  end;

end;

procedure TFilterForm.OnFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := DataSet.FieldByName('Amount').AsFloat >= FAmount;
  //Accept := DataSet.FieldByName('Name').AsString = 'Teo';
end;


procedure TFilterForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TFilterForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

