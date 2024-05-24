unit c_ScopeTest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  ,SysUtils

  ,Tripous.Logs
  ;

type

  { TDeveloper }

  TDeveloper = class
  private
    FLogSource: ILogSource;

    procedure Develop();
    procedure Debug();
    procedure Deploy();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure TestLogSource();
  end;

implementation

{ TDeveloper }

constructor TDeveloper.Create();
begin
  inherited Create;
  FLogSource := Logger.CreateLogSource(Self.ClassName);

  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()');
  FLogSource.Info('LogSource created');
end;

destructor TDeveloper.Destroy;
begin
  FLogSource.ExitScope();
  inherited Destroy;
end;

procedure TDeveloper.TestLogSource();
begin
  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()' );
  FLogSource.Info('Testing');
  Develop();
  Deploy();
  FLogSource.Info('Test is done');
  FLogSource.ExitScope();
end;

procedure TDeveloper.Develop();
begin
  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()' );
  FLogSource.Info('Developing');
  Debug();
  FLogSource.Info('Developing is done');
  FLogSource.ExitScope();
end;

procedure TDeveloper.Debug();
begin
  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()' );
  FLogSource.Info('Debugging');
  FLogSource.ExitScope();
end;

procedure TDeveloper.Deploy();
begin
  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()' );
  FLogSource.Info('Deploying');
  FLogSource.ExitScope();
end;

end.

