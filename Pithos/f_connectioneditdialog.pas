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
    FConInfoProxy: TSqlConInfoProxy;
    IsInitialized: Boolean;

    procedure AnyClick(Sender: TObject);
    procedure CheckConnection();
    procedure CreateDatabase();

    procedure ItemToControls();
    procedure ControlsToItem();

    function  AssignItem(): Boolean;
  protected
    procedure Activate; override;
  public
    class function ShowDialog(ConInfo: TSqlConInfoProxy): Boolean;
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
    ConInfo := FConInfoProxy.CreateSqlConnectionInfo();
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
  edtName.Text := FConInfoProxy.Name;
  if FConInfoProxy.Provider <> '' then
     cboProvider.ItemIndex := cboProvider.Items.IndexOf(FConInfoProxy.Provider);
  edtServer.Text := FConInfoProxy.Server;
  edtDatabase.Text := FConInfoProxy.Database;
  edtUserName.Text := FConInfoProxy.UserName;
  edtPassword.Text := FConInfoProxy.Password;
  mmoParams.Clear();
  if Assigned(FConInfoProxy.Params) then
     mmoParams.Lines.Assign(FConInfoProxy.Params);

  edtName.Enabled := Sys.IsEmpty(FConInfoProxy.Name);
end;

procedure TConnectionEditDialog.ControlsToItem();
begin
  if AssignItem() then
     Self.ModalResult := mrOK;
end;

function TConnectionEditDialog.AssignItem(): Boolean;
begin
  FConInfoProxy.Name      := Trim(edtName.Text);
  FConInfoProxy.Provider  := cboProvider.Text;
  FConInfoProxy.Server    := Trim(edtServer.Text);
  FConInfoProxy.Database  := Trim(edtDatabase.Text);
  FConInfoProxy.UserName  := Trim(edtUserName.Text);
  FConInfoProxy.Password  := Trim(edtPassword.Text);
  FConInfoProxy.Params.Clear();
  FConInfoProxy.Params.Assign(mmoParams.Lines);


  Result := not (Sys.IsEmpty(FConInfoProxy.Name)
                or Sys.IsEmpty(FConInfoProxy.Provider)
                or Sys.IsEmpty(FConInfoProxy.Server)
                or Sys.IsEmpty(FConInfoProxy.Database)
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

class function TConnectionEditDialog.ShowDialog(ConInfo: TSqlConInfoProxy): Boolean;
var
  F: TConnectionEditDialog;
begin
  F := TConnectionEditDialog.Create(Application);
  try
    F.FConInfoProxy := ConInfo;
    Result := F.ShowModal() = mrOK;
  finally
    F.Free();
  end;
end;

end.

