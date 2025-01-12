unit dm_Common;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TdmCommon }

  TdmCommon = class(TDataModule)
    ImageListTreeView: TImageList;
    ImageListInspector: TImageList;
    ImageList24: TImageList;

  private

  public

  end;

var
  dmCommon: TdmCommon;

implementation

{$R *.lfm}

{ TdmCommon }



{ TdmCommon }



initialization
   dmCommon := TdmCommon.Create(nil);

end.

