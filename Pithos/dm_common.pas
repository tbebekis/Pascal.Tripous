unit dm_Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TdmCommon }

  TdmCommon = class(TDataModule)
    CommonImageList: TImageList;
  private

  public

  end;

var
  dmCommon: TdmCommon;

implementation

{$R *.lfm}

initialization
   dmCommon := TdmCommon.Create(nil);

end.

