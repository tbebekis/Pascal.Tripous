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

  ;

type
  TConDialogMode = (cdmNone = 0, cdmInsert = 1, cdmEdit = 2, cdmCreateDb = 4);

  { TConnectionEditDialog }

  TConnectionEditDialog = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnCheckConnection: TButton;
    btnCreateDatabase: TButton;
    cboProvider: TComboBox;
    edtName: TEdit;
    edtServer: TEdit;
    edtDatabase: TEdit;
    edtUserName: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    mmoParams: TMemo;
  private
    ConInfoProxy: TSqlConInfoProxy;
    IsInitialized: Boolean;
    Mode: TConDialogMode;

    procedure AnyClick(Sender: TObject);
    procedure CheckConnection();
    procedure CreateDatabase();

    procedure ItemToControls();
    procedure ControlsToItem();

    function  AssignItem(): Boolean;
  protected
    procedure Activate; override;
  public
    class function ShowDialog(ConInfoProxy: TSqlConInfoProxy; Mode: TConDialogMode): Boolean;
  end;


implementation

{$R *.lfm}

{ TConnectionEditDialog }

procedure TConnectionEditDialog.AnyClick(Sender: TObject);
begin
       if btnOK = Sender then ControlsToItem()
  else if btnCheckConnection = Sender then CheckConnection()
  else if btnCreateDatabase = Sender then CreateDatabase()
  ;
end;

procedure TConnectionEditDialog.CheckConnection();
var
  ConInfo: TSqlConnectionInfo;
begin
  if AssignItem() then
  begin
    ConInfo := ConInfoProxy.CreateSqlConnectionInfo();
    try
       if ConInfo.CanConnect(False) then
          ShowMessage('Success.')
       else
          ShowMessage('Faild to connect to database.');
    finally
      ConInfo.Free();
    end;
  end;
end;

procedure TConnectionEditDialog.CreateDatabase();
begin
  // TODO:
end;

procedure TConnectionEditDialog.ItemToControls();
begin
  edtName.Text := ConInfoProxy.Name;
  if ConInfoProxy.Provider <> '' then
     cboProvider.ItemIndex := cboProvider.Items.IndexOf(ConInfoProxy.Provider);
  edtServer.Text := ConInfoProxy.Server;
  edtDatabase.Text := ConInfoProxy.Database;
  edtUserName.Text := ConInfoProxy.UserName;
  edtPassword.Text := ConInfoProxy.Password;
  mmoParams.Clear();
  if Assigned(ConInfoProxy.Params) then
     mmoParams.Lines.Assign(ConInfoProxy.Params);

  // ReadOnly
  cboProvider.Enabled := not (Mode in [cdmEdit]);
  edtServer.Enabled := not (Mode in [cdmEdit]);
  edtDatabase.Enabled := not (Mode in [cdmEdit]);

  btnCreateDatabase.Enabled := Mode = cdmCreateDb;
end;

procedure TConnectionEditDialog.ControlsToItem();
begin
  if AssignItem() then
     Self.ModalResult := mrOK;
end;

function TConnectionEditDialog.AssignItem(): Boolean;
begin
  ConInfoProxy.Name      := Trim(edtName.Text);
  ConInfoProxy.Provider  := cboProvider.Text;
  ConInfoProxy.Server    := Trim(edtServer.Text);
  ConInfoProxy.Database  := Trim(edtDatabase.Text);
  ConInfoProxy.UserName  := Trim(edtUserName.Text);
  ConInfoProxy.Password  := Trim(edtPassword.Text);
  ConInfoProxy.Params.Clear();
  ConInfoProxy.Params.Assign(mmoParams.Lines);


  Result := not (Sys.IsEmpty(ConInfoProxy.Name)
                or Sys.IsEmpty(ConInfoProxy.Provider)
                or Sys.IsEmpty(ConInfoProxy.Server)
                or Sys.IsEmpty(ConInfoProxy.Database)
                );

  if not Result then
     ShowMessage('Connection Info not fully defined.');
end;

procedure TConnectionEditDialog.Activate;
var
  S: string;
begin
  inherited Activate;

  if not IsInitialized then
  begin
    IsInitialized := True;

    Self.CancelControl  := btnCancel;
    Self.DefaultControl := btnOK;

    btnOK.OnClick  := AnyClick;
    btnCheckConnection.OnClick  := AnyClick;
    btnCreateDatabase.OnClick  := AnyClick;

    cboProvider.Items.Clear();
    for S in SqlProviders.ProviderNames do
      cboProvider.Items.Add(S);

    cboProvider.ItemIndex := 0;
    ItemToControls();
  end;
end;

class function TConnectionEditDialog.ShowDialog(ConInfoProxy: TSqlConInfoProxy; Mode: TConDialogMode): Boolean;
var
  F: TConnectionEditDialog;
begin
  F := TConnectionEditDialog.Create(Application);
  try
    F.ConInfoProxy := ConInfoProxy;
    F.Mode := Mode;
    Result := F.ShowModal() = mrOK;
  finally
    F.Free();
  end;
end;

end.

