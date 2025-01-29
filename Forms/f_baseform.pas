unit f_BaseForm;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    TFormMode = ( fmNormal
                 ,fmModal
                 ,fmChild
                 ,fmMdiChild
                 );

  { TFormInitInfo }
  TFormInitInfo = class
  private
    FCaption: string;
    FFormMode: TFormMode;
    FParent: TWinControl;
    FTag: TObject;
    FvTag: Variant;
  public
    property Caption: string read FCaption write FCaption;
    property FormMode: TFormMode read FFormMode write FFormMode;
    property Parent: TWinControl read FParent write FParent;
    property Tag: TObject read FTag write FTag;
    property vTag: Variant read FvTag write FvTag;
  end;

  { TBaseForm }
  TBaseForm = class(TForm)
  private class var
    FAutoIncId : Integer;
  private
    FFormId: Integer;

    function GetIsChild: Boolean;
    function GetIsModal: Boolean;

    procedure SetupFormMode();
  protected
    FIsInitialized : Boolean;
    FInitInfo: TFormInitInfo;

    procedure DoShow; override;
    procedure TextChanged(); override;

    procedure FormInitialize(); virtual;
    procedure EnableCommands(); virtual;
    function  GetFormId: Integer;
    procedure AnyClick(Sender: TObject); virtual;
  public
    property IsInitialized: Boolean read FIsInitialized write FIsInitialized;

    constructor Create(Owner: TComponent; InitInfo: TFormInitInfo); overload; virtual;
    destructor Destroy(); override;

    property FormId: Integer read GetFormId;
    property IsChild: Boolean read GetIsChild;
    property IsModal: Boolean read GetIsModal;
  end;

  TBaseFormClass = class of TBaseForm;



implementation

{$R *.lfm}

{ TBaseForm }

constructor TBaseForm.Create(Owner: TComponent; InitInfo: TFormInitInfo);
begin
  inherited Create(Owner);

  KeyPreview   := True;

  Inc(FAutoIncId);
  FFormId := FAutoIncId;

  if not Assigned(InitInfo) then
    InitInfo := TFormInitInfo.Create();

  FInitInfo := InitInfo;
end;

destructor TBaseForm.Destroy();
begin
  FInitInfo.Free();
  inherited Destroy();
end;

procedure TBaseForm.SetupFormMode();
begin
  case FInitInfo.FormMode of
    fmNormal    : ;
    fmModal     : begin
                    Position       := poMainFormCenter;
                    BorderStyle    := bsSizeable;
                    BorderIcons    := Self.BorderIcons - [biMinimize];
                  end;
    fmChild     : begin
                    BorderStyle    := bsNone;
                    Parent         := FInitInfo.Parent;
                    Align          := alClient;
                    Visible        := True;
                    SetFocus();
                    Realign();         // fix for Delphi 4 (???)
                  end;
    fmMdiChild  : begin
                    FormStyle      := fsMDIChild;
                  end;
  end;

  if FInitInfo.Caption <> '' then
    Caption := FInitInfo.Caption
  else
    Caption := 'Form ' + IntToStr(FFormId);
end;

procedure TBaseForm.DoShow;
begin
  SetupFormMode();

  inherited DoShow;

  if not IsInitialized then
  try
    FormInitialize();
  finally
    FIsInitialized := True;
  end;

end;

procedure TBaseForm.FormInitialize();
begin
end;

procedure TBaseForm.EnableCommands();
begin
end;

procedure TBaseForm.TextChanged();
begin
  inherited TextChanged();

  if IsChild then
    Parent.Caption := Caption;
end;

function  TBaseForm.GetIsModal: Boolean;
begin
  Result := (fsModal in Self.FormState) and (FInitInfo.FormMode = fmMdiChild);
end;

function TBaseForm.GetIsChild: Boolean;
begin
  Result := Assigned(Parent) and (FInitInfo.FormMode = fmChild);
end;

function TBaseForm.GetFormId: Integer;
begin
  Result := FFormId;
end;

procedure TBaseForm.AnyClick(Sender: TObject);
begin
  EnableCommands();
end;



end.

