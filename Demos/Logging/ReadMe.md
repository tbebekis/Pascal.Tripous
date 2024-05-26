# Event Log
 
There is a number of [Event Logging](https://en.wikipedia.org/wiki/Logging_(computing)) systems for [Free Pascal](https://www.freepascal.org/) and [Lazarus](https://www.lazarus-ide.org/).

- [LazLogger](https://wiki.freepascal.org/LazLogger)
- [Log4Delphi](https://wiki.freepascal.org/Log4Delphi)
- [Log4Pascal](https://github.com/martinusso/log4pascal)
- [MultiLog](https://wiki.freepascal.org/MultiLog)

Is there really a need for another one logging framework in Pascal? Well, I believe there is.

## A simple and efficient Logging framework

This text describes the Tripous Logging system, which is enough simple and easy to use.

A logging framework can be implemented around a static Logger class.



That static `Logger` class provides a set of static log methods.

```
  Logger = class
  public
    class procedure Log(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);

    class procedure Debug(Source, ScopeId, EventId, Text: string);
    class procedure Debug(Source, EventId, Text: string);
    class procedure Debug(Source, Text: string);
    class procedure Debug(Text: string);

    class procedure Info(Source, ScopeId, EventId, Text: string);
    class procedure Info(Source, EventId, Text: string);
    class procedure Info(Source, Text: string);
    class procedure Info(Text: string);

    class procedure Warning(Source, ScopeId, EventId, Text: string);
    class procedure Warning(Source, EventId, Text: string);
    class procedure Warning(Source, Text: string);
    class procedure Warning(Text: string);

    class procedure Error(Source, ScopeId, EventId: string; Ex: Exception);
    class procedure Error(Source, EventId: string; Ex: Exception);
    class procedure Error(Source: string; Ex: Exception);
    class procedure Error(Ex: Exception);

    class procedure Error(Source, ScopeId, EventId: string; Text: string);
    class procedure Error(Source, EventId: string; Text: string);
    class procedure Error(EventId: string; Text: string);
    class procedure Error(Text: string); 

    class property Active: Boolean read GetActive write SetActive;
    class property MinLevel: TLogLevel read GetMinLevel write SetMinLevel;   	
  end; 
```

## The TLogLevel, the MinLevel and the Active property

The `TLogLevel` is defined as following.

```
type
  TLogLevel = (
     loNone      = 0
    ,loTrace     = 1
    ,loDebug     = 2
    ,loInfo      = 4
    ,loWarning   = 8
    ,loError     = $10
    ,loFatal     = $20
  );  
```

Log information is produced by the `Logger` class, and further processed, if and only if the `Logger.Active` is `True`, which is the default, and the call to the `Log(...)` method passes a `Level` greater than or equal to the `Logger.MinLevel`.

## The TLogEntry

The `Logger.Log(...)` method examines the passed parameters and generates a unit of log information, an instance of a `TLogEntry` class.

```
  TLogEntry = class(TInterfacedObject, ILogEntry)
  public
    constructor Create(Source, ScopeId, EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    destructor Destroy(); override;

    property Id: string read GetId;
    property TimeStamp: TDateTime read  GetTimeStamp;
    property Date: string read GetDate;
    property Time: string read GetTime;
    property User: string read GetUser write SetUser;
    property Host: string read GetHost;

    property Level: TLogLevel read GetLevel;
    property Source: string read GetSource;
    property ScopeId: string read GetScopeId;
    property EventId: string read GetEventId;
    property Text: string read GetText;
    property Exception_ : Exception read GetException;
    property Properties: IVariantDictionary read GetProperties;    
  end;  
```

## The TLogListener(s)

Then the `Logger` class passes that newly created `TLogEntry` instance to a number of registered log listeners for further processing, **asynchronously**, meaning **using a thread**. 

The `Logger` static class keeps a list of registered log listeners and calls them when a new `TLogEntry` is created, passing the enty.

A log listener inherits from a base `TLogListener` and it needs just a single virtual method:

```
  TLogListener = class
  protected
    procedure ProcessLog(const Entry: ILogEntry); virtual; abstract;
  public
    constructor Create();
    destructor Destroy(); override;
  end;   
```

One log listener may display the log information to a console, while another log listener may save that information to a text file or a database table.

That way functionality to persist or display the log information goes to the log listeners.

And the developer may implement and use its own log listeners when needed, just inheriting from `TLogListener`.

There is no need to manually register a `TLogListener` with the `Logger` class. The `TLogListener` constructor registers the listener while its destructor unregisters the listener.

## Source and Scope

The `Source` and the `Scope` of a `TLogEntry` are just names devised by the developer, and could be just empty strings.

A `Source` could be the name of a class, such as a `TMainForm` or a similar code construction that is considered as a module in an application. 

A `Scope` could be the name of a method or function or the name of a group of functions, or whatever.

The `Logger` class provides a method to create a `Source`.

```
class function CreateLogSource(SourceName: string): ILogSource;     
```

Here is the `Source`.

```
  ILogSource = interface
    ['{019F8BB3-E92F-4580-A3BD-A142FFA0B28A}']
    function  GetActive: Boolean;
    procedure SetActive(AValue: Boolean);
    function  GetName: string;
    procedure SetName(AValue: string);

    procedure EnterScope(Id: string; const ScopeParams: IVariantDictionary = nil);
    procedure ExitScope();

    procedure Log(EventId: string; Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Exception_: Exception; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(EventId: string; Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);
    procedure Log(Level: TLogLevel; Text: string; const Params: IVariantDictionary = nil);

    procedure Debug(EventId, Text: string);
    procedure Debug(Text: string);

    procedure Info(EventId, Text: string);
    procedure Info(Text: string);

    procedure Warning(EventId, Text: string);
    procedure Warning(Text: string);

    procedure Error(EventId: string; Ex: Exception);
    procedure Error(Ex: Exception);

    procedure Error(EventId: string; Text: string);
    procedure Error(Text: string);

    { properties }
    property Name: string read GetName write SetName;
    property Active: Boolean read GetActive write SetActive;
  end;   
```

The `Source` creates a `Default Scope` when it is created. The developer **never** gets a reference to a `Scope`.

The developer may call `EnterScope(...)` passing it a `Scope` name.

The `ExitScope()` exits the last entered `Scope`. The `Default Scope` cannot exited.

It is even safe to call `ExitScope()` more times than the existing `Scopes`.

Any call to `Source` log methods, such as `Log()`, `Debug()`, `Error()`, etc. produces a `TLogEntry` marked with the `Source` name and the name of the latest  `Scope`.

Here is how to use a `Source` and a `Scope`.

```

FLogSource := Logger.CreateLogSource(Self.ClassName);        

...

procedure TDeveloper.Develop();
begin
  FLogSource.EnterScope({$I %CURRENTROUTINE%} + '()' ); // this gets the name of the method
  FLogSource.Info('Developing');
  WriteCode();
  Debug();
  Test();
  FLogSource.Info('Developing is done');
  FLogSource.ExitScope();
end; 
```

## `TLogEntry` Properties and Params

A number of `Log()` methods, of the `Logger` class and the `ILogSource` interface, accept a parameter of type `IVariantDictionary`, under the name `Params`.

Subsequently the `TLogEntry` class has a `Properties` property of type `IVariantDictionary`.

An `IVariantDictionary` can be created using the `TVariantDictionary` class, found in `Tripous.pas` code unit.

```
   TVariantDictionary = class(TInterfacedObject, IVariantDictionary)
   public
     constructor Create();
     destructor Destroy(); override;

     procedure Add(const Key: string; const Value: Variant);
     function  Remove(const Key: string): Boolean;
     procedure Clear();

     function  ContainsKey(const Key: string): Boolean;
     function  ContainsValue(const Value: Variant): Boolean;

     function  GetKeys: TArrayOfString;
     function  GetValues: TArrayOfVariant;

     function IndexOfKey(const Key: string): Integer;
     function ItemByKey(const Key: string): TKeyValue;
     function IndexOfValue(const Value: Variant): Integer;

     function GetEnumerator(): TVariantDictionaryEnumerator;

     property Count: Integer read GetCount;
     property Item[const Key: string]: Variant read GetValue write SetValue; default;
     property Keys: TArrayOfString read GetKeys;
     property Values: TArrayOfVariant read GetValues;
   end; 
```

The `TVariantDictionary` is a dictionary-like class. 

It is an [enumerable](https://www.freepascal.org/docs-html/ref/refsu59.html) class, i.e. provides a `GetEnumerator()` method and thus it can be used in a `for .. in .. do` statement.

The item type of the `TVariantDictionary` dictionary is the following.

```
   TKeyValue = class
   public
     Key : string;
     Value : Variant;
     constructor Create(AKey: string; AValue: Variant);
   end;   
```

It is safe to use the `TVariantDictionary.Item[]` default array property in order to add a `Value` even when the `Key` does **not** already  exist.

Here is an example of use along with a `Source`.

```
procedure TMainForm.LogSourceTest();
var
  LogSource: ILogSource;
  Params: IVariantDictionary;
begin
  // simple
  LogSource := Logger.CreateLogSource(Self.ClassName);
  LogSource.Info(GetLogText());

  // with params
  Params := TVariantDictionary.Create();
  Params['CustomerId'] := 'BigCo';
  Params['OrderId']    := 123;

  LogSource.Log(loInfo, 'Customer {CustomerId} Order with Id {OrderId} is completed.', Params);
end;   
```

In the above code the `{CustomerId}` and `{OrderId}` are placeholders for the values passed by the `IVariantDictionary`.

The final log message would be `Customer BigCo Order with Id 123 is completed.`

## Log text variations

The `Logger` and the `TLogEntry` may provide the log information, to any log listener or other client code, in 3 textual forms.

- List
- Line
- Json

```
    property AsList : string read GetAsList;
    property AsLine : string read GetAsLine;
    property AsJson : string read GetAsJson; 
```

### List
```
Id          : {A0E66074-317F-438F-9164-24EAC06542B5}
TimeStamp   : 2024-05-25 21:44:31.976
Level       : Info 
Source      : TMainForm
Scope       : Default Scope
EventId     : 0
Host        : TEO
User        : tbebekis
Text        : Customer BigCo Order with Id 123 is completed.
Properties  :  
CustomerId = BigCo
OrderId = 123
```

### Json

```
{
  "Date": "2024-05-25",
  "EventId": "0",
  "Host": "TEO",
  "Id": "{A0E66074-317F-438F-9164-24EAC06542B5}",
  "Level": "Info ",
  "Message": "Customer BigCo Order with Id 123 is completed.",
  "Properties": "CustomerId = BigCo, OrderId = 123",
  "Scope": "Default Scope",
  "Source": "TMainForm",
  "Time": "21:44:31.976",
  "TimeStamp": 45437.90592564815,
  "User": "tbebekis"
}
```

## Predefined `TLogListener` classes

There is a list of predefined listener classes.

- `TFileLogListener`. Writes log information to a log (text) file. Each `TLogEntry` occupies a single line of text. The first line contains the captions.
- `TSqlDbLogListener`. Writes log information to a database table. Any database supported by Lazarus [SqlDB](https://wiki.freepascal.org/SQLdb_Tutorial1) may used.
- `TFormLogListener`. Shows a `TForm` when created where it displays log information in a memo and in a grid.
- `TLogTextListener`. Passes log information, as list of text lines, to a callback method.
- `TLogLineListener`. Passes log information, as a single line of text, to a callback method.
 
## Retain policy

The `Logger` class provides 3 properties regarding retain policy.

- `RetainDays: Integer`. How many days to retain the log information in the storage medium. Defaults to 7.
- `RetainSizeKiloBytes: SizeInt`. How many KB to allow a single log file to grow. Defaults to 512 KB.
- `RetainPolicyCounter: Integer`. After how many log writes to check whether it is time to apply the retain policy. Defaults to 100.

The `TLogListener` has the exact same properties. Its constructor uses the corresponding `Logger` properties as the get the default values for its own properties.

A descendant `TLogListener` may or may not involve retain policy.

The predefined log listeners respect these settings though.

## Demo application
There is a demo application displaying 
- the use of all predefined listeners, except `TLogLineListener`
- how to use `Source` and `Scope`
- how to use Properties/Params with the `IVariantDictionary`

## Usage
To use `Logger` in an application a developer has to add two code units in the `uses` clause.

- `Tripous.pas`
- `Tripous.Logs.pas`

No other dependencies exist.

To actually use `Logger` just create a `TLogListener` and then call any of `Logger` methods.

```
  FFileLogListener := TFileLogListener.Create();   

  ...

  Logger.Warning('This is too easy');
```

The use of `Source` and `Scope` is **not** mandatory. They should be used just when they actually needed.

## Tested On
- FPC 3.2.2
- Lazarus 3.0




  









