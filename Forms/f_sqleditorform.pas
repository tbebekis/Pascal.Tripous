unit f_SqlEditorForm;

{$mode DELPHI}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ComCtrls
  //, StdCtrls
  , SynHighlighterSQL
  , SynEdit

  , Tripous
  , Tripous.Data
  , Tripous.Logs

  ,f_BaseForm
  ;

type
  { TSqlEditorForm }
  TSqlEditorForm = class(TBaseForm)
    btnExit: TToolButton;
    ImageList: TImageList;
    mmoText: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar: TToolBar;
  private
    FInitialSql: string;
    FPage: TTabSheet;
  public
    constructor Create(Owner: TComponent; InitInfo: TFormInitInfo); overload; override;
    destructor Destroy(); override;

    procedure AnyClick(Sender: TObject);  override;

    class function CreateAsChild(Pager: TPageControl; Caption: string; InitialSql: string): TSqlEditorForm;
    class function CreateAsChildForMetadata(Pager: TPageControl; MetaNode: TMetaNode): TSqlEditorForm;
    class function CreateAsChildForFieldList(Pager: TPageControl; MetaNode: TMetaNode): TSqlEditorForm;

    property Page: TTabSheet  read FPage;
    property InitialSql: string   read FInitialSql;
    property Id: Integer read GetFormId;
  end;



implementation

{$R *.lfm}

{ TSqlEditorForm }
class function TSqlEditorForm.CreateAsChild(Pager: TPageControl; Caption: string; InitialSql: string): TSqlEditorForm;
var
  InitInfo: TFormInitInfo;
  Page : TTabSheet;
begin
  Page  := Pager.AddTabSheet();
  InitInfo := TFormInitInfo.Create();
  InitInfo.FormMode := fmChild;
  InitInfo.Parent   := Page;
  InitInfo.Caption  := Caption;
  InitInfo.vTag     := InitialSql;

  Result := TSqlEditorForm.Create(Page, InitInfo);
  Result.Visible := True;
  Pager.ActivePage := Page;
end;

class function TSqlEditorForm.CreateAsChildForMetadata(Pager: TPageControl; MetaNode: TMetaNode): TSqlEditorForm;
var
  InitialSql: string;
  Title: string;
begin
  InitialSql := '';
  Title := '';

  if (MetaNode.NodeType = ntTable) or (MetaNode.NodeType = ntView) then
  begin
    Title := MetaNode.Name + ' Fields';
    if (MetaNode.NodeType = ntTable) then
       InitialSql := TMetaTable(MetaNode).Fields.GetFieldListText(False)
    else
       InitialSql := TMetaView(MetaNode).Fields.GetFieldListText(False);
  end;

  Result := CreateAsChild(Pager, Title, InitialSql);
end;

class function TSqlEditorForm.CreateAsChildForFieldList(Pager: TPageControl; MetaNode: TMetaNode): TSqlEditorForm;
var
  InitialSql: string;
  Title: string;
begin
  Title := MetaNode.Name + ' Definition';
  InitialSql := MetaNode.GetDefinition();

  Result := CreateAsChild(Pager, Title, InitialSql);
end;

constructor TSqlEditorForm.Create(Owner: TComponent; InitInfo: TFormInitInfo);
begin
  inherited Create(Owner, InitInfo);

  FPage := InitInfo.Parent as TTabSheet;
  FInitialSql := InitInfo.vTag;

  Page.Caption := IntToStr(Id) + '. ' + InitInfo.Caption;
  btnExit.OnClick := AnyClick;
  mmoText.Clear();
  mmoText.Text := InitialSql;
end;

destructor TSqlEditorForm.Destroy();
begin
  inherited Destroy();
end;

procedure TSqlEditorForm.AnyClick(Sender: TObject);
begin
  if btnExit = Sender then
  begin
    FPage.Free();
  end else begin

  end;
end;





end.

