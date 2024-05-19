unit frm_LocateLookupForm;

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

  { TLocateLookupForm }

  TLocateLookupForm = class(TForm)
    btnLocate: TButton;
    btnLookup: TButton;
    edtKeyField: TEdit;
    edtKeyValue: TEdit;
    Grid: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
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



procedure TLocateLookupForm.InitializeTest();
var
  i: Integer;
begin
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Id', ftAutoInc);
  Table.FieldDefs.Add('Name', ftString, 100);

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

  btnLocate.OnClick := @AnyClick;
  btnLookup.OnClick := @AnyClick;
end;

procedure TLocateLookupForm.AnyClick(Sender: TObject);
var
  Res : Variant;
begin
  if (btnLocate = Sender) then
  begin
    if not Table.Locate(edtKeyField.Text, StrToInt(edtKeyValue.Text), []) then
       ShowMessage('Locate failed')
    else
       ShowMessage('OK');
  end else if (btnLookup = Sender) then
  begin
    // Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
    Res := Table.Lookup(edtKeyField.Text, StrToInt(edtKeyValue.Text), 'Name');
    ShowMessage(Res);
  end;

end;

procedure TLocateLookupForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TLocateLookupForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

