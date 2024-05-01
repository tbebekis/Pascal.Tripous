unit frm_BlobForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , ExtCtrls
  , Graphics
  , Dialogs
  , Menus
  , DB
  , DBCtrls
  , DBGrids

  ,TypInfo

  ,O_App
  ,Tripous.MemTable
  ;

type

  { TBlobForm }

  TBlobForm = class(TForm)
    imgImage: TDBImage;
    mnuImageLoadFromFile: TMenuItem;
    mnuImageSaveToFile: TMenuItem;
    mnuImageClear: TMenuItem;
    mnuImageCopyToClipboard: TMenuItem;
    mnuImagePasteFromClipboard: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mmoMemo: TDBMemo;
    Grid: TDBGrid;
    Panel2: TPanel;
    mnuImage: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private
    DS: TDatasource;
    Table: TMemTable;
    procedure InitializeTest();
    procedure AnyClick(Sender: TObject);

    procedure ImageLoadFromFile();
    procedure ImageSaveToFile();
    procedure ImageClear();
    procedure ImageCopyToClipboard();
    procedure ImagePasteFromClipboard();
  protected
    procedure KeyPress(var Key: char); override;
    procedure DoShow; override;
  end;



implementation

{$R *.lfm}


procedure TBlobForm.InitializeTest();
var
  i: Integer;
begin
  Table := TMemTable.Create(Self);
  Table.FieldDefs.Add('Id', ftAutoInc);
  Table.FieldDefs.Add('Name', ftString, 100);
  Table.FieldDefs.Add('Memo', ftMemo);
  Table.FieldDefs.Add('Image', ftGraphic);
  Table.CreateDataset;

  DS := TDataSource.Create(Self);
  DS.DataSet := Table;
  Grid.DataSource := DS;

  Table.Active := True;

  App.AdjustGridColumns(Grid);

  Table.FieldByName('Id').ReadOnly := True;

  for i := 0 to 1 do
  begin
    Table.Append();
    Table.FieldByName('Name').AsString := 'Name_' + IntToStr(i + 1);
    Table.Post();
  end;

  Table.First;

  imgImage.DataSource := DS;
  imgImage.DataField := 'Image';

  mmoMemo.DataSource := DS;
  mmoMemo.DataField := 'Memo';

  mnuImageLoadFromFile.OnClick := @AnyClick;
  mnuImageSaveToFile.OnClick := @AnyClick;
  mnuImageClear.OnClick := @AnyClick;
  mnuImageCopyToClipboard.OnClick := @AnyClick;
  mnuImagePasteFromClipboard.OnClick := @AnyClick;

end;

procedure TBlobForm.AnyClick(Sender: TObject);
begin
  if mnuImageLoadFromFile = Sender then
     ImageLoadFromFile()
  else if mnuImageSaveToFile  = Sender then
     ImageLoadFromFile()
  else if mnuImageClear  = Sender then
     ImageClear()
  else if mnuImageCopyToClipboard  = Sender then
     ImageCopyToClipboard()
  else if mnuImagePasteFromClipboard  = Sender then
     ImagePasteFromClipboard()
     ;

end;

procedure TBlobForm.ImageLoadFromFile();
var
  Dlg : TOpenDialog;
begin
  if (Table.Active) and (Table.RecordCount > 0) then
  begin
    Dlg := TOpenDialog.Create(nil);
    try
      Dlg.Filter := 'JPEG|*.jpg|PNG|*.png|GIF|*.gif|BMP|*.bmp';
      if Dlg.Execute() then
      begin
        Table.Edit;
        TGraphicField(Table.FieldByName('Image')).LoadFromFile(Dlg.FileName);
      end;
    finally
      Dlg.Free;
    end;
  end;
end;

procedure TBlobForm.ImageSaveToFile();
var
  Dlg: TSaveDialog;
begin
  if (Table.Active) and (Table.RecordCount > 0) then
  begin
    if not Table.FieldByName('Image').IsNull then
    begin
      Dlg := TSaveDialog.Create(nil);
      try
        Dlg.Filter := 'JPEG|*.jpg|PNG|*.png|GIF|*.gif|BMP|*.bmp';
        if Dlg.Execute() then
          TGraphicField(Table.FieldByName('Image')).SaveToFile(Dlg.FileName);
      finally
        Dlg.Free;
      end;
    end;
  end;
end;

procedure TBlobForm.ImageClear();
begin
  if (Table.Active) and (Table.RecordCount > 0) then
  begin
    Table.Edit;
    Table.FieldByName('Image').Clear;
  end;
end;

procedure TBlobForm.ImageCopyToClipboard();
begin
  if (Table.Active) and (Table.RecordCount > 0) then
  begin
    imgImage.CopyToClipboard();
  end;
end;

procedure TBlobForm.ImagePasteFromClipboard();
begin
  if (Table.Active) and (Table.RecordCount > 0) then
  begin
    imgImage.PasteFromClipboard();
  end;
end;

procedure TBlobForm.KeyPress(var Key: char);
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;

  inherited KeyPress(Key);
end;

procedure TBlobForm.DoShow;
begin
  inherited DoShow;
  KeyPreview := True;
  Position := poMainFormCenter;

  InitializeTest();
end;

end.

