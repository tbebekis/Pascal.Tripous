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
      FImageList     : TImageList;
      FMap           : TStringList;   // Name=Index (Index in Objects as PtrInt)
      FValueTypes    : TStringList;   // kept for compatibility (not used by RCDATA loader)
      FResourceNames : TStringList;   // the list of RCDATA resource names to load
      FLoaded        : Boolean;

    class function  NormalizeValueType(const S: string): string; static;
    class function  IsAllowedValueType(const VT: string): Boolean; static;
    class function  FindRow(const Name: string): Integer; static;

    class procedure LoadFromResources; static;
    class procedure EnsureResourceNames; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure Clear;

    { Must be called once (typically at app startup) }
    class procedure SetResourceNames(const Names: array of string); overload;
    class procedure SetResourceNames(Names: TStrings); overload;

    class procedure Load;

    class function IndexOf(const IconName: string): Integer;
    class function Has(const IconName: string): Boolean;

    class function AddButton(AToolBar: TToolBar; const AIconName: string; const AHint: string; AOnClick: TNotifyEvent): TToolButton;
    class function AddSeparator(AToolBar: TToolBar): TToolButton;

    class property ImageList: TImageList read FImageList;
    class property ValueTypes: TStringList read FValueTypes; // legacy
  end;

implementation

uses
  Graphics, LCLType; // RT_RCDATA

{ IconList }

class constructor IconList.Create;
begin
  // Created once
  FImageList := TImageList.Create(nil);
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

  // Default allowed types (legacy - not used by RCDATA loader)
  FValueTypes.Add('PNG');
  FValueTypes.Add('GIF');
  FValueTypes.Add('JPG');
  FValueTypes.Add('JPEG');
  FValueTypes.Add('BMP');

  FResourceNames := TStringList.Create;
  FResourceNames.CaseSensitive := False;
  FResourceNames.Sorted := True;
  FResourceNames.Duplicates := dupIgnore;

  FLoaded := False;
end;

class destructor IconList.Destroy;
begin
  FResourceNames.Free;
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

class procedure IconList.SetResourceNames(const Names: array of string);
var
  i: Integer;
  S: string;
begin
  FResourceNames.Clear;
  for i := Low(Names) to High(Names) do
  begin
    S := Trim(Names[i]);
    if S <> '' then
      FResourceNames.Add(S);
  end;

  // force reload next time
  FLoaded := False;
end;

class procedure IconList.SetResourceNames(Names: TStrings);
var
  i: Integer;
  S: string;
begin
  FResourceNames.Clear;
  if Names <> nil then
    for i := 0 to Names.Count - 1 do
    begin
      S := Trim(Names[i]);
      if S <> '' then
        FResourceNames.Add(S);
    end;

  FLoaded := False;
end;

class procedure IconList.EnsureResourceNames;
begin
  if FResourceNames.Count = 0 then
    raise Exception.Create('IconList resource names not set. Call IconList.SetResourceNames(...) at startup.');
end;

class procedure IconList.LoadFromResources;
var
  i: Integer;
  ResName: string;
  RS: TResourceStream;
  Pic: TPicture;
  Bmp: TBitmap;
  Index: Integer;
begin
  if (FImageList.Width <= 0) or (FImageList.Height <= 0) then
    raise Exception.Create('IconList.ImageList.Width/Height must be set before Load().');

  EnsureResourceNames;
  Clear;

  Pic := TPicture.Create;
  Bmp := TBitmap.Create;
  try
    for i := 0 to FResourceNames.Count - 1 do
    begin
      ResName := Trim(FResourceNames[i]);
      if ResName = '' then
        Continue;

      RS := nil;
      try
        // RCDATA resources (Project Options -> Resources)
        RS := TResourceStream.Create(HInstance, ResName, RT_RCDATA);

        Pic.Clear;
        Pic.LoadFromStream(RS);
        if (Pic.Graphic = nil) or Pic.Graphic.Empty then
          Continue;

        Bmp.Assign(Pic.Graphic);

        // If you want strict size policy, enable this check
        //if (Bmp.Width <> FImageList.Width) or (Bmp.Height <> FImageList.Height) then
        //  Continue;

        Index := FImageList.Add(Bmp, nil);

        FMap.AddObject(
          ResName + '=' + IntToStr(Index),
          TObject(PtrInt(Index))
        );
      except
        // ignore missing/invalid resources (or change to raise if you prefer)
      end;

      RS.Free;
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
  Result.AutoSize := True;
end;

end.
