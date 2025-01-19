unit fr_SqlEditorFrame;

{$MODE DELPHI}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ComCtrls
  , StdCtrls, SynHighlighterSQL, SynEdit

  , Tripous
  , Tripous.Data
  , Tripous.Logs
  ,o_App
  ;

type

  { TSqlEditorFrame }
  TSqlEditorFrame = class(TFrame)
    btnExit: TToolButton;
    ImageList: TImageList;
    mmoText: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
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

{ TSqlEditorFrame }



constructor TSqlEditorFrame.Create(Page: TTabSheet; Title: string; InitialText: string);
begin
  inherited Create(Page);
  FPage := Page;
  FInitialText := InitialText;
  Parent := Page;
  Align := alClient;
  FId := App.NextTextPageId();

  Page.Caption := IntToStr(Id) + '. ' + Title;

  btnExit.OnClick := AnyClick;

  mmoText.Clear();

  mmoText.Text := InitialText;
end;

destructor TSqlEditorFrame.Destroy();
begin
  inherited Destroy();
end;

procedure TSqlEditorFrame.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then
  begin
    FPage.Free();
  end else begin

  end;
end;

end.

