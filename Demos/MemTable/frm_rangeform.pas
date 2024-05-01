unit frm_RangeForm;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
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

  { TRangeForm }

  TRangeForm = class(TForm)
    btnApplyRange: TButton;
    btnCancelRange: TButton;
    edtFieldName: TEdit;
    edtStartValue: TEdit;
    edtEndValue: TEdit;
    Grid: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
  private
    DS: TDatasource;
    Table: TMemTable;
    procedure InitializeTest();
    procedure AnyClick(Sender: TObject);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}


procedure TRangeForm.InitializeTest();
var
  i: Integer;
begin
  // master
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Id', ftAutoInc);
  Table.FieldDefs.Add('Name', ftString, 100);
  //Table.FieldDefs.Add('Data', ftString, 100);
  Table.CreateDataset;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

  Table.Active := True;

  App.AdjustGridColumns(Grid);

  Table.FieldByName('Id').ReadOnly := True;

  for i := 0 to 9 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    Table.Post();
  end;

  btnApplyRange.OnClick := @AnyClick;
  btnCancelRange.OnClick := @AnyClick;
end;

procedure TRangeForm.AnyClick(Sender: TObject);
begin
  if (btnApplyRange = Sender) then
  begin
    Table.SetRange(edtFieldName.Text, StrToInt(edtStartValue.Text), StrToInt(edtEndValue.Text), false);
  end else if (btnCancelRange = Sender) then
  begin
    Table.CancelRange();
  end;

end;

procedure TRangeForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TRangeForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

