unit f_ConnectionEditDialog;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ExtCtrls
  , StdCtrls

  , Tripous
  , Tripous.Data

  ,o_App
  ;

type

  { TConnectionEditDialog }

  TConnectionEditDialog = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cboProvider: TComboBox;
    edtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmoConnectionString: TMemo;
  private
    FConInfo: TConInfoProxy;
    IsInitialized: Boolean;
    procedure AnyClick(Sender: TObject);
    procedure ItemToControls();
    procedure ControlsToItem();
  protected
    procedure DoShow; override;
  public
    class function ShowDialog(ConInfo: TConInfoProxy): Boolean;
  end;


implementation

{$R *.lfm}

{ TConnectionEditDialog }

procedure TConnectionEditDialog.AnyClick(Sender: TObject);
begin
  if btnOK = Sender then ControlsToItem()
  ;
end;

procedure TConnectionEditDialog.ItemToControls();
begin
  edtName.Text := FConInfo.Name;
  if FConInfo.Provider <> '' then
     cboProvider.ItemIndex := cboProvider.Items.IndexOf(FConInfo.Provider);
  mmoConnectionString.Text := FConInfo.ConnectionString;

  edtName.Enabled := Sys.IsEmpty(FConInfo.Name);
end;

procedure TConnectionEditDialog.ControlsToItem();
begin
  Self.ModalResult := mrOK;
end;

procedure TConnectionEditDialog.DoShow;
var
  S: string;
begin
  inherited DoShow;

  if not IsInitialized then
  begin
    IsInitialized := True;

    Self.CancelControl  := btnCancel;
    Self.DefaultControl := btnOK;

    btnOK.OnClick  := AnyClick;

    cboProvider.Items.Clear();
    for S in SqlProviders.ProviderNames do
      cboProvider.Items.Add(S);

    cboProvider.ItemIndex := 0;
    ItemToControls();
  end;
end;

class function TConnectionEditDialog.ShowDialog(ConInfo: TConInfoProxy): Boolean;
var
  F: TConnectionEditDialog;
begin
  F := TConnectionEditDialog.Create(Application);
  try
    F.FConInfo := ConInfo;
    Result := F.ShowModal() = mrOK;
  finally
    F.Free();
  end;
end;

end.

