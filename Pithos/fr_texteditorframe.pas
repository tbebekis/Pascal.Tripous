unit fr_TextEditorFrame;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ComCtrls
  , StdCtrls

  , Tripous
  , Tripous.Data
  , Tripous.Logs
  ,o_App
  ;

type

  { TTextEditorFrame }

  TTextEditorFrame = class(TFrame)
    btnExit: TToolButton;
    ImageList: TImageList;
    mmoText: TMemo;
    ToolBar: TToolBar;
  private
    FId: Integer;
    FInitialText: string;
    FPage: TTabSheet;
    procedure AnyClick(Sender: TObject);
  public
    constructor Create(Page: TTabSheet; Title: string;  InitialText: string); overload;
    destructor Destroy(); override;

    property Page: TTabSheet  read FPage;
    property InitialText: string   read FInitialText;
    property Id: Integer read FId;
  end;

implementation

{$R *.lfm}

{ TTextEditorFrame }

constructor TTextEditorFrame.Create(Page: TTabSheet; Title: string; InitialText: string);
begin
  inherited Create(Page);
  FPage := Page;
  FInitialText := InitialText;
  Parent := Page;
  Align := alClient;
  FId := App.NextTextPageId();

  Page.Caption := IntToStr(Id) + '. ' + Title;

  btnExit.OnClick := AnyClick;

  mmoText.Text := InitialText;
end;

destructor TTextEditorFrame.Destroy();
begin
  inherited Destroy();
end;

procedure TTextEditorFrame.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then
  begin
    FPage.Free();
  end else begin

  end;
end;

end.

