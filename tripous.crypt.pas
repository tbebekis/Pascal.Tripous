unit Tripous.Crypt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  Encryptor = class
  public
    { encrypt/decrypt }
    class function Encrypt(Passphrase: string; PlainText: string): string;
    class function Decrypt(Passphrase: string; EncryptedText: string): string;
  end;

implementation

uses
   DCPrc4        // dcpcrypt package is required
  ,DCPsha256     // dcpcrypt package is required
  ;

class function Encryptor.Encrypt(Passphrase: string; PlainText: string): string;
var
  Cipher : TDCP_rc4;
begin
  Cipher := TDCP_rc4.Create(nil);
  try
    Cipher.InitStr(Passphrase, TDCP_sha256);
    Result := Cipher.EncryptString(PlainText);
  finally
    Cipher.Free;
  end;
end;

class function Encryptor.Decrypt(Passphrase: string; EncryptedText: string): string;
var
  Cipher : TDCP_rc4;
begin
  Cipher := TDCP_rc4.Create(nil);
  try
    Cipher.InitStr(Passphrase, TDCP_sha256);
    Result := Cipher.DecryptString(EncryptedText);
  finally
    Cipher.Free;
  end;

end;


end.

