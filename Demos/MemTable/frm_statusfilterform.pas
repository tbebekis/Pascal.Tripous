unit frm_StatusFilterForm;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
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

  { TStatusFilterForm }

  TStatusFilterForm = class(TForm)
    btnApplyFilter: TButton;
    btnCancelUpdates: TButton;
    chUnmodified: TCheckBox;
    chModified: TCheckBox;
    chInserted: TCheckBox;
    chDeleted: TCheckBox;
    Grid: TDBGrid;
    lblStatus: TLabel;
    Panel1: TPanel;
  private
    DS: TDatasource;
    Table: TMemTable;
    procedure InitializeTest();
    procedure AnyClick(Sender: TObject);
    procedure Table_OnAfterScroll(DataSet: TDataSet);
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}



procedure TStatusFilterForm.InitializeTest();
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

  for i := 0 to 2 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    Table.Post();
  end;

  btnApplyFilter.OnClick := @AnyClick;
  btnCancelUpdates.OnClick := @AnyClick;

  Table.AfterScroll := @Table_OnAfterScroll;

  Table.CancelUpdates();

end;

procedure TStatusFilterForm.AnyClick(Sender: TObject);
var
  StatusFilter : TUpdateStatusSet;
begin
  if (btnApplyFilter = Sender) then
  begin
    StatusFilter := [];
    if chUnmodified.Checked then StatusFilter := StatusFilter + [usUnmodified];
    if chModified.Checked   then StatusFilter := StatusFilter + [usModified];
    if chInserted.Checked   then StatusFilter := StatusFilter + [usInserted];
    if chDeleted.Checked    then StatusFilter := StatusFilter + [usDeleted];

    Table.StatusFilter := StatusFilter;
  end else if (btnCancelUpdates = Sender) then
  begin
    Table.CancelUpdates();
  end;

end;
procedure TStatusFilterForm.Table_OnAfterScroll(DataSet: TDataSet);
var
  Status : TUpdateStatus;
begin
  Status := Table.UpdateStatus;

  lblStatus.Caption := GetEnumName(TypeInfo(TUpdateStatus), Ord(Status));
end;
procedure TStatusFilterForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TStatusFilterForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

