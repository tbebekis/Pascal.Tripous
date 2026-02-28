unit Tripous.IconList;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ImgList;

type
  { IconList }
  IconList = class
  private
    class var
      FImageList  : TImageList;
      FMap        : TStringList;   // Name=Index (Index in Objects as PtrInt)
      FValueTypes : TStringList;
      FLoaded     : Boolean;

    class function  NormalizeValueType(const S: string): string; static;
    class function  IsAllowedValueType(const VT: string): Boolean; static;
    class function  FindRow(const Name: string): Integer; static;
    class procedure LoadFromResources; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure Clear;
    class procedure Load;

    class function IndexOf(const IconName: string): Integer;
    class function Has(const IconName: string): Boolean;

    class function AddButton(AToolBar: TToolBar; const AIconName: string; const AHint: string; AOnClick: TNotifyEvent): TToolButton;
    class function AddSeparator(AToolBar: TToolBar): TToolButton;

    class property ImageList: TImageList read FImageList;
    class property ValueTypes: TStringList read FValueTypes;
  end;

implementation

uses
  Graphics, LResources;

{ IconList }

class constructor IconList.Create;
begin
  // Created once
  FImageList := TImageList.Create(nil);
  //FImageList.Scaled := True;
  FImageList.Height := 24;
  FImageList.Width := 24;

  FMap := TStringList.Create;
  FMap.CaseSensitive := False;
  FMap.Sorted := True;
  FMap.Duplicates := dupIgnore;
  FMap.NameValueSeparator := '=';

  FValueTypes := TStringList.Create;
  FValueTypes.CaseSensitive := False;
  FValueTypes.Sorted := True;
  FValueTypes.Duplicates := dupIgnore;

  // Default allowed types
  FValueTypes.Add('PNG');
  FValueTypes.Add('GIF');
  FValueTypes.Add('JPG');
  FValueTypes.Add('JPEG');
  FValueTypes.Add('BMP');

  FLoaded := False;
end;

class destructor IconList.Destroy;
begin
  FValueTypes.Free;
  FMap.Free;
  FImageList.Free;
end;

class procedure IconList.Clear;
begin
  FLoaded := False;
  FMap.Clear;
  FImageList.Clear;
end;

class function IconList.NormalizeValueType(const S: string): string;
begin
  Result := UpperCase(Trim(S));
end;

class function IconList.IsAllowedValueType(const VT: string): Boolean;
begin
  Result := FValueTypes.IndexOf(NormalizeValueType(VT)) >= 0;
end;

class function IconList.FindRow(const Name: string): Integer;
begin
  Result := -1;
  if FMap.Count = 0 then Exit;
  Result := FMap.IndexOfName(Name);
end;

class procedure IconList.LoadFromResources;
var
  i: Integer;
  Res: TLResource;
  Pic: TPicture;
  Bmp: TBitmap;
  Index: Integer;
begin
  if (FImageList.Width <= 0) or (FImageList.Height <= 0) then
    raise Exception.Create('IconList.ImageList.Width/Height must be set before Load().');

  Clear;

  Pic := TPicture.Create;
  Bmp := TBitmap.Create;
  try
    for i := 0 to LazarusResources.Count - 1 do
    begin
      Res := LazarusResources.Items[i];

      if not IsAllowedValueType(Res.ValueType) then
        Continue;

      Pic.Clear;
      Pic.LoadFromLazarusResource(Res.Name);
      if (Pic.Graphic = nil) or Pic.Graphic.Empty then
        Continue;

      Bmp.Assign(Pic.Graphic);

      // Strict size policy     [teo: no let the image list resize the icon]
      //if (Bmp.Width <> FImageList.Width) or
      //   (Bmp.Height <> FImageList.Height) then
      //  Continue;

      Index := FImageList.Add(Bmp, nil);

      FMap.AddObject(
        Res.Name + '=' + IntToStr(Index),
        TObject(PtrInt(Index))
      );
    end;

    FLoaded := True;
  finally
    Bmp.Free;
    Pic.Free;
  end;
end;

class procedure IconList.Load;
begin
  if FLoaded then Exit;
  LoadFromResources;
end;

class function IconList.IndexOf(const IconName: string): Integer;
var
  Row: Integer;
begin
  Load;

  Result := -1;
  if Trim(IconName) = '' then Exit;

  Row := FindRow(IconName);
  if Row < 0 then Exit;

  Result := PtrInt(FMap.Objects[Row]);
end;

class function IconList.Has(const IconName: string): Boolean;
begin
  Result := IndexOf(IconName) >= 0;
end;

class function IconList.AddButton(AToolBar: TToolBar; const AIconName: string; const AHint: string; AOnClick: TNotifyEvent): TToolButton;
var
  ImgIndex: Integer;
begin
  if not Assigned(AToolBar) then
    raise Exception.Create('AToolBar is nil.');

  Load; // ensure icons loaded

  // ensure toolbar uses our imagelist
  if AToolBar.Images <> FImageList then
    AToolBar.Images := FImageList;

  ImgIndex := IndexOf(AIconName);
  //if ImgIndex < 0 then
  //  raise Exception.CreateFmt('Icon "%s" not found.', [AIconName]);


  Result := TToolButton.Create(AToolBar);
  Result.Parent := AToolBar;
  Result.Style := tbsButton;
  Result.ImageIndex := ImgIndex;
  Result.Hint := AHint;
  Result.ShowHint := AHint <> '';
  Result.OnClick := AOnClick;

  Result.BringToFront();
end;
class function IconList.AddSeparator(AToolBar: TToolBar): TToolButton;
begin
  if not Assigned(AToolBar) then
    raise Exception.Create('AToolBar is nil.');

  Result := TToolButton.Create(AToolBar);
  Result.Parent := AToolBar;
  Result.Style := tbsSeparator;
  Result.AutoSize := True; // αφήνει το toolbar να ορίσει το πλάτος
end;


end.

