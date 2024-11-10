unit Tripous;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
   Classes
  ,SysUtils
  ,StrUtils
  ,DateUtils
  ,FileUtil
  ,LazSysUtils
  ,Controls
  ,Graphics
  ,SyncObjs
  ,base64
  ,Variants
  ,TypInfo
  ,DB
  ,bufdataset
  ,laz2_DOM
  ,fpjsonrtti

  //, Laz2_XMLUtils


  ;





const
  CRLF = #13#10;
  LB   = #13#10;  { LineBreak }
  NULL_STR   = '##null##';

  SetOfChars = ['a'..'z', 'A'..'Z'];
  SetOfNumbers = ['0'..'9'];
(*----------------------------------------------------------------------------*)

const
(*----------------------------------------------------------------------------
  Encodings for Greek Language
   iso-8859-7
   windows-1253

 <?xml version="1.0" encoding="iso-8859-7" standalone="yes"?>
 ----------------------------------------------------------------------------*)


 EncodingUtf8 = 'utf-8' ;
 EncodingUtf16 = 'utf-16';
 EncodingGreek = 'iso-8859-7';



(*----------------------------------------------------------------------------
const
  ctNone         = 'N';
  ctString       = 'S';
  ctWideString   = 'W';
  ctInteger      = 'I';
  ctBoolean      = 'B';
  ctFloat        = 'F';
  ctCurrency     = 'C';
  ctDate         = 'D';
  ctTime         = 'T';
  ctDateTime     = 'M';
  ctEnum         = 'E';
  ctSet          = 'L';
  ctMemo         = 'X';
  ctGraphic      = 'G';
  ctBlob         = 'H';
  ctReference    = 'O';
  ctInterface    = 'U';
  *)


type



 TArrayOfVariant   = array of Variant;
 TArrayOfVarRec    = array of TVarRec;
 TArrayOfPointer   = array of Pointer;
 TArrayOfString    = array of string;

 TProcedureMethod  = procedure of object;

 ISyncObject = interface(IInterface)
   ['{B2411903-F6D1-4FF3-9DA6-E7FDCFE3A702}']
 { public }
   procedure Lock;
   procedure UnLock;
 end;

 { TSafeObjectList }
 TSafeObjectList = class
 private
   FList        : TList;
   FLock        : SyncObjs.TCriticalSection;
   FOwnsObjects : Boolean;

   function  GetCount: Integer;
   function  GetIsEmpty: Boolean;
   function GetItem(Index: Integer): TObject;
   procedure Lock;
   procedure UnLock;
 public
   constructor Create(OwnsObjects: Boolean);
   destructor Destroy; override;

   procedure Add(Instance: TObject);
   procedure Insert(Index: Integer; Instance: TObject);
   procedure Remove(Instance: TObject);
   procedure RemoveAt(Index: Integer);
   function  Contains(Instance: TObject): Boolean;
   procedure Clear();

   procedure Push(Instance: TObject);
   function  Pop(): TObject;

   procedure Sort(Compare: TListSortCompare);

   property Count: Integer read GetCount;
   property Items[Index: Integer]: TObject read GetItem; default;
   property IsEmpty: Boolean read GetIsEmpty;
 end;

   { TKeyValue }
   TKeyValue = class
   public
     Key : string;
     Value : Variant;
     constructor Create(AKey: string; AValue: Variant);
   end;

   TVariantDictionary = class;

   { TVariantDictionaryEnumerator }
   TVariantDictionaryEnumerator = class
   private
     FDictionary: TVariantDictionary;
     FPosition : Integer;
   public
     constructor Create();

     function GetCurrent: TKeyValue;
     procedure Reset;

     function MoveNext: boolean;
     property Current: TKeyValue read GetCurrent;
   end;


   IVariantDictionary = interface
   ['{0ECC7A7B-5105-40C1-805F-C5EBD4C1D8FA}']
     function GetCount: Integer;
     function  GetValue(const Key: string): Variant;
     procedure SetValue(const Key: string; Value: Variant);

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

   { TVariantDictionary }
   TVariantDictionary = class(TInterfacedObject, IVariantDictionary)
   private
     FList : TList;
     function GetCount: Integer;
     function  GetValue(const Key: string): Variant;
     procedure SetValue(const Key: string; Value: Variant);
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

   { TWriteLineFile }
   TWriteLineFile = class
   private
     function GetRetainSizeKiloBytes: SizeInt;
   protected
     FIsClosed             : Boolean;
     F                     : TextFile;

     FFolder               : string;
     FDefaultFileName      : string;
     FLastFileName         : string;
     FColumnLine           : string;

     FSize                 : SizeInt;
     FRetainSizeKiloBytes  : SizeInt;

     procedure BeginFile();
     procedure CreateFile(FilePath: string);
     procedure CloseFile();
   public
     constructor Create(FilePath: string = ''; aColumnLine: string = '');
     destructor Destroy(); override;

     procedure WriteLine(Line: string);
     procedure DeleteFilesOlderThan(Days: Integer = 1);

     property Folder               : string read FFolder;
     property DefaultFileName      : string read FDefaultFileName;
     property LastFileName         : string read FLastFileName;
     property ColumnLine           : string read FColumnLine;
     property Size                 : SizeInt read FSize;

     property IsClosed             : Boolean read FIsClosed;
     property RetainSizeKiloBytes  : SizeInt read GetRetainSizeKiloBytes write FRetainSizeKiloBytes;
   end;

   { TBufTable }
   TBufTable = class(TBufDataset)
   private
     FTableName : string;
     FFixedSource : TDataSource;
   protected
     procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;

     procedure SetActive (Value : Boolean); override;
   public
     constructor Create(AOwner: TComponent); override;

     procedure SaveToFileXml(FilePath: string);

     { properties }
     property TableName   : string read FTableName write FTableName;
     property FixedSource : TDataSource read FFixedSource;
   end;

  { Reflection }
  Reflection = class
  public
    { info }
    class function  IsSameType(Info1, Info2: PTypeInfo): Boolean;
    class function  IsSubRange(Info: PTypeInfo): Boolean;
    class function  IsOrdinal(Info: PTypeInfo): Boolean;
    class function  InheritsFrom(Instance: TObject; const aClassName: string; InfoList: TStrings = nil): Boolean;

    class function  UnitNameOf(Info: PTypeInfo): string;
    class function  BaseOf(Info: PTypeInfo): PTypeInfo;
    class function  TypeSizeOf(Info: PTypeInfo): Integer;

    { get/set properties: see TypInfo functions }
    class function  HasProperty(Instance: TObject; const PropertyName: string): Boolean;

    { set }
    class function  SetValueToInt(Info: PTypeInfo; const SetVar): Integer;
    class procedure IntToSetValue(Info: PTypeInfo; var SetVar; const Value: Integer);
    class function  SetValueToStr(Info: PTypeInfo; const SetVar; const Brackets: Boolean): string;
    class procedure StrToSetValue(Info: PTypeInfo; var SetVar; const Value: string);
    class function  SetLiteralsToStr(Info: PTypeInfo): string;

    { enum }
    class procedure EnumTypeToList(Info: PTypeInfo; List: TStrings; PrefixCut: Integer = 0);
    class function  EnumValueToStr(Info: PTypeInfo; Value: Integer; PrefixCut: Integer = 0): string;
    class function  EnumLiteralsMinIndex(Info: PTypeInfo): Integer;
    class function  EnumLiteralsMaxIndex(Info: PTypeInfo): Integer;
    class function  EnumLiteralsToStr(Info: PTypeInfo): string;
    class function  EnumLiteralByIndex(Info: PTypeInfo; Index: Integer): string;
    class function  EnumIndexOfLiteral(Info: PTypeInfo; const Name: string): Integer;

    { events }
    class function  EventsEqual(A, B: TMethod): Boolean;
    class function  NotifyEventsEqual(A, B: TNotifyEvent): Boolean;
    class function  ReplaceEvent(TargetObject: TObject; TargetEventName: string; NewObject: TObject; NewEventName: string): TMethod;
    class function  ReplaceEvent2(TargetObject: TObject; TargetEventName: string; NewObject: TObject; NewEventMethod: Pointer): TMethod;

    { component }
    class function  ComponentToString(Component: TComponent): string;
    class function  StringToComponent(Owner: TComponent; Parent: TComponent; Component: TComponent; Value: string): TComponent;
    class procedure ReadFormTextResource(TextResource: string; Form: TComponent);
    class procedure CreatePascalFiles(Form: TComponent; aUnitName: string = '');
  end;

  { Json }
  Json = class
  public
    const DefaultStreamOptions = [
        //jsoStreamChildren,         // If set, children will be streamed in 'Children' Property
        //jsoEnumeratedAsInteger,    // Write enumerated as integer. Default is string.
        //jsoSetAsString,            // Write Set as a string. Default is an array.
        //jsoSetEnumeratedAsInteger, // Write enumerateds in set array as integers.
        jsoSetBrackets,            // Use brackets when creating set as array
        //jsoComponentsInline,       // Always stream components inline. Default is to stream name, unless csSubcomponent in ComponentStyle
        jsoTStringsAsArray,        // Stream TStrings as an array of strings. Associated objects are not streamed.
        //jsoTStringsAsObject,       // Stream TStrings as an object : string = { object }
        jsoDateTimeAsString,       // Format a TDateTime value as a string
        jsoUseFormatString,        // Use FormatString when creating JSON strings.
        jsoCheckEmptyDateTime     // If TDateTime value is empty and jsoDateTimeAsString is used, 0 date returns empty string
        //jsoLegacyDateTime,         // Set this to enable old date/time formatting. Current behaviour is to save date/time as a ISO 9601 value.
        //jsoLowerPropertyNames,     // Set this to force lowercase names when streaming to JSON.
        //jsoStreamTList             // Set this to assume that TList contains a list of TObjects. Use with care!
    ];
    const DefaultDestreamOptions = [
        jdoCaseInsensitive,
        //jdoIgnorePropertyErrors,
        jdoIgnoreNulls
        //jdoNullClearsProperty
    ];

    class function  Serialize(Instance: TObject; Options: TJSONStreamOptions = DefaultStreamOptions): string;
    class procedure Deserialize(Instance: TObject; JsonText: string; Options: TJSONDestreamOptions = DefaultDestreamOptions);

    class procedure LoadFromFile(FilePath: string; Instance: TObject);
    class procedure SaveToFile(FilePath: string; Instance: TObject);
  end;

  { Xml }
  Xml = class
  public
    class function  IsValidName(Name: string): Boolean;
    class function  NormalizeTypeName(Name: string): string;

    class function  VariantToXml(const Value: Variant): string;  { VariantToXML convert a value to the valid XML string }
    class function  StrToXml(const S: string): string;           { StrToXml converts '<', '>', '"', cr, lf characters to its ascii codes }
    class function  XmlToStr(Value: Variant): string;            { XmlToStr is opposite to StrToXml function }

    { create and load/save }
    class function  CreateDoc(RootName: string = 'root'; Encoding : string = EncodingUtf8): TXMLDocument;

    class function  TextToDoc(const XmlText: string): TXMLDocument;
    class function  DocToText(Doc: TXMLDocument): string;

    class function  StreamToDoc(Stream: TStream): TXMLDocument;
    class function  DocToStream(Doc: TXMLDocument): TStream;

    class procedure SaveToFile(Doc: TXMLDocument; FilePath: string);
    class function  LoadFromFile(FilePath: string): TXMLDocument;

    { get/set values etc. }
    class function  AddNode(Parent: TDOMNode; ChildName: string; Value: Variant): TDOMElement; overload;
    class function  AddNode(Parent: TDOMNode; ChildName: string): TDOMElement; overload;

    class function  AddCData(Parent: TDOMNode; ChildName: string; CDataText: string): TDOMElement;

    class function  GetNodeValue(const Node: TDOMNode; DefaultValue: string = ''): string;
    class procedure SetNodeValue(const Node: TDOMNode; Value: Variant);

    class function  GetChild(const Parent: TDOMNode; ChildName: string; DefaultValue: string = ''): string;
    class procedure SetChild(const Parent: TDOMNode; ChildName: string; Value: Variant);

    class function  GetAttr(const Node: TDOMNode; AttrName: string; DefaultValue: string = ''): string;
    class procedure SetAttr(const Node: TDOMNode; AttrName: string; Value: Variant);

    class function  GetCData(const Node: TDOMNode; DefaultValue: string): string; overload;
    class procedure SetCData(const Node: TDOMNode; Value: string); overload;

    class function  GetCData(const Parent: TDOMNode; ChildName: string; DefaultValue: string): string; overload;
    class procedure SetCData(const Parent: TDOMNode; ChildName: string; Value: string); overload;

    { XML - dataset to/from XML }
    class procedure DatasetSchemaToXml(Table: TDataset; TopNode: TDOMElement);
    class procedure XmlToDatasetSchema(Table: TDataset; TopNode: TDOMElement);

    class procedure DatasetDataToXml(Table: TDataset; TopNode: TDOMElement);
    class procedure XmlToDatasetData(Table: TDataset; TopNode: TDOMElement);

    class procedure DatasetToXml(Table: TDataset; TopNode: TDOMElement); overload;
    class function  DatasetToXml(Table: TDataset; Encoding: string = EncodingUtf8): TXMLDocument; overload;

    class function  DatasetToXmlText(Table: TDataset; Encoding: string = EncodingUtf8): string;
    class procedure XmlTextToDataset(Table: TDataset; XmlText: string; CreateDataset: TProcedureMethod);

    class procedure XmlToDataset(Table: TDataset; TopNode: TDOMElement; CreateDataset: TProcedureMethod); overload;
    class procedure XmlToDataset(Table: TDataset; Doc: TXMLDocument; CreateDataset: TProcedureMethod); overload;

    class procedure DatasetToXmlFile(Table: TDataset; const FileName: string; Encoding : string = EncodingUtf8);
    class procedure DatasetFromXmlFile(Table: TDataset; const FileName: string; CreateDataset: TProcedureMethod);

    class procedure DatasetToXmlStream(Table: TDataset; Stream: TStream; Encoding : string = EncodingUtf8);
    class procedure DatasetFromXmlStream(Table: TDataset; Stream: TStream; CreateDataset: TProcedureMethod);
  end;


  TGraphicType = ( gtNone
                  ,gtICO
                  ,gtBMP
                  ,gtJPG
                  );

  Depictor = class(TObject)
  public
    class function  GetGraphicType(Stream: TStream): TGraphicType;
    class function  LoadGraphicFromStream(Stream: TStream; var Graphic: TGraphic): TGraphicType;
    class function  LoadPictureFromStream(Stream: TStream; Picture: TPicture): TGraphicType;
    class function  LoadPictureFromField(Field: TBlobField; Picture: TPicture): TGraphicType;
  end;

  (*----------------------------------------------------------------------------
  A utility class that converts binary to hexadecimal data and the opposite.

  Among other things it provides BinToHex() overloads as a facility
  in creating hex string lines of the same length, with or without surrounding quotes,
  in order to assign string constants when writing code. That is
  a bitmap or anything else it can be converted to a set of string lines
  and then paste those lines into the souce code.
  ----------------------------------------------------------------------------*)

  { HexConverter }

  HexConverter = class(TObject)
  public
      { Strips any characters other than HexDigits from Text. }
    class function  NormalizeHexText(Text: string): string;
    class function  StripLineBreaks(Text: string; LineBreak: string): string; overload;
    class function  StripLineBreaks(Text: string): string; overload;
    class function  InsertLineBreaks(Text: string; LineLength: Integer; Quoted: Boolean): string;

    class function  BinToHex(Buffer: Pointer; Size: UInt32): string; overload;
    class function  BinToHex(Stream: TStream): string; overload;
    class function  BinToHex(Field: TBlobField): string; overload;
    class function  BinToHex(Stream: TStream; LineLength: Integer; Quoted: Boolean): string; overload;
    class procedure BinToHex(Stream: TStream; List: TStrings; LineLength: Integer; Quoted: Boolean); overload;
    class procedure BinToHex(Graphic: TGraphic; List: TStrings; LineLength: Integer; Quoted: Boolean); overload;
    class procedure BinToHex(Bitmap: TBitmap; List: TStrings; LineLength: Integer; Quoted: Boolean); overload;

    { Returns the number of bytes written to the stream }
    class function  HexToBin(Text: string; Stream: TStream): Integer; overload;
    class function  HexToBin(Text: string; Field: TBlobField): Integer; overload;
    class function  HexToBin(Text: string; var Graphic: TGraphic): Boolean; overload;
    class function  HexToBin(Text: string; var Bitmap: TBitmap): Boolean; overload;
    class function  HexToBin(Text: string; Picture: TPicture): Boolean; overload;

    class function  StrToHex(const Text: string): string;
    class function  HexToStr(const Text: string): string;

    class function LoadGraphicFromStream(Stream: TStream; var Graphic: TGraphic): Boolean;
  end;

(*----------------------------------------------------------------------------
 A TObject is not directly assignable to a Variant.
 An Interface is.
 The IObjectBox is a facility for wrapping TObject objects
 inside an IInterface object, in order to assign it to a Variant.
 ----------------------------------------------------------------------------*)
  IObjectBox = interface(IInterface)
  ['{E502D3BC-E0AD-40C0-963C-E92BCAB68059}']
    function get_Instance: TObject;
    function get_OwnsInstance: Boolean;

    property Instance    : TObject read get_Instance;
    property OwnsIstance : Boolean read get_OwnsInstance;
  end;

  { Sys }
  Sys = class
  private class var

    FInvariantFormatSettings    : TFormatSettings;

    FAppFolder                  : string;

    FAppPath                    : string;
    FAppExeName                 : string;
  private
    class var FProcessMessagesMethod: TProcedureMethod;
    class function GetAppDataFolder: string; static;
  public
    { construction }
    class constructor Create();
    class destructor Destroy();

    { exceptions }
    class procedure Error(const Msg: string); overload;
    class procedure Error(const Msg: string; const Args: array of const); overload;
    class procedure ErrorNotYet(const Msg: string);


    { log }
    //class procedure LogSqlError(E: Exception; const SqlText: string);


    { Base64 encoding }
    class function StringToBase64(Input: string): string;
    class function StreamToBase64(Input: TStream): string;
    class function Base64ToString(Input: string): string;
    class function Base64ToStream(Input: string): TStream;

    { strings }
    class function IsSameText(A: string; B: string): Boolean;
    class function IsEmpty(const S: string): Boolean;
    class function PathCombine(A: string; B: string): string;
    class function IsLetter(C: Char): Boolean;

    class function  LCopy(const Text: string; const Index: Integer): string;
    class function  RCopy(const Text: string; const Index: Integer): string;
    class function  LPad(const S: string; C: WideChar; Len: Integer): string;
    class function  RPad(const S: string; C: WideChar; Len: Integer): string;

    class function  Split(S: string; Delim: Char):  TStringList;  overload;
    class procedure Split(S: string; Delim: Char; List: TStringList); overload;
    class procedure Split(S: string; Delim: string; var LeftArg: string; var RightArg: string); overload;

    class function  QS(const S: string): string;     { QuotedStr }
    class function  AppendCRLF(const S: string): string;
    class function  StripCRLFs(const S: string): string ;

    class function  UnicodeToAnsi(const S: string): AnsiString;

    class function  LoadFromFile(const FileName: string): string;
    class procedure SaveToFile(const FileName: string; const Data: string);
    class procedure WriteToFile(const FileName, S: string);

    class function  LoadTextFromStream(Stream: TStream): string;
    class procedure SaveTextToStream(Stream: TStream; Data: string);

    class function  IsValidFileName(const FileName: string): Boolean;
    class function  StrToValidFileName(const S: string): string;
    class function  CreateGuid(UseBrackets: Boolean): string;

    class function  PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;

    { double to string convertions }
    class function  DoubleToStr(Value: Extended; Digits: integer): string;
    class function  DoubleToStrSQL(Value: Extended; Digits: integer): string;
    class function  FormatMoney(Value: Extended; bCurrencyString : Boolean): string;
    class function  NormalDecSep(const S : string): string;
    class function  DotToComma(const sFloat : string): string;
    class function  CommaToDot(const sFloat : string): string;

    { date-time safe }
    { two tries to get the value, first in local and then in global format settings }
    class function  StrToDateSafe(const S: string; Default: TDate = 0): TDate;
    class function  StrToTimeSafe(const S: string; Default: TTime = 0): TTime;
    class function  StrToDateTimeSafe(const S: string; Default: TDateTime = 0): TDateTime;

    { datetime }
    class function  DateToStrSQL(ADate: TDateTime; bQuoted : boolean = True): string;
    class function  TimeToStrSQL(ATime: TDateTime; bQuoted : boolean = True): string;
    class function  DateTimeToStrSQL(ADateTime: TDateTime; bQuoted : boolean = True): string;
    class function  TimeStamp(ADateTime: TDateTime; bQuoted : boolean = True): string;
    class function  ExtractDate(DT: TDateTime): TDateTime;
    class function  ExtractTime(DT: TDateTime): TDateTime;

    class function  DateTimeToFileName(const DT: TDateTime; UseMSecs: Boolean = False): string;
    class function  DateTimeToFileName2(const DT: TDateTime; UseMSecs: Boolean = False): string;

    { variants }
    class function  VarIsNull(const Value: Variant): Boolean;
    class function  VarIsValid(const Value: Variant): Boolean;
    class function  VarIsDisp(const V: Variant): Boolean;
    class function  VarIsUnk(const V: Variant): Boolean;
    class function  VarIsObj(const V: Variant): Boolean;
    class function  VarEquals(const V1, V2: Variant): Boolean;
    class function  VarCompare(const Item1, Item2: Variant): Integer;
    class function  VarToArray(const Source: Variant): TArrayOfVariant;

    { boxing-unboxing plain TObject objects into a Variant }
    class function BoxObject(Instance: TObject; OwnsIstance: Boolean = False): IObjectBox;
    class function Box(Instance: TObject; OwnsIstance: Boolean = False): Variant;
    class function UnBox(Value: Variant): TObject;
    class function IsBox(const Value: Variant): Boolean;

    { arrays }
    class function  StreamToVarArray(const Stream: TStream): Variant;
    class procedure VarArrayToStream(const V: Variant; const Stream: TStream);
    class function  ByteArrayToVariant(const A: TBytes): Variant;
    class function  VariantToByteArray(const V: Variant): TBytes;
    class procedure ByteArrayToStream(const A: TBytes; const Stream: TStream);
    class function  StreamToByteArray(const Stream: TStream): TBytes;
    class function  CopyByteArray(const A: TBytes): TBytes;

    { file utils }
    class function  CombinePath(const A, B: string): string;
    class function  NormalizePath(const Path: string): string;
    class function  DenormalizePath(const Path: string): string;
    class function  EnsureExtension(FileName: string; Extension: string): string;

    class function  FolderDelete(Folder: string): Boolean;
    class function  FolderCopy(Source, Dest: string; Overwrite: Boolean = True): Boolean;
    class function  FolderMove(Source, Dest: string; Overwrite: Boolean = True): Boolean;

    class procedure FindFiles(StartFolder, FileMask: string; List: TStrings; AddPath: Boolean = False; Recursive: Boolean = False);

    class function  HasAttribute(const FileName: string; FileAttr: integer): boolean;
    class function  GetFileDate(const FileName: string): TDateTime;
    class function  SetFileDate(const FileName: string; NewDate: TDateTime): boolean;
    class function  GetFileSize(const FileName: string): Int64;
    class function  GetFolderSize(const Path: string): Int64;

    { stream utils }
    class procedure WS(Stream: TStream; V: Integer); overload;
    class procedure WS(Stream: TStream; V: Double);  overload;
    class procedure WS(Stream: TStream; V: String);  overload;
    class function  RS(Stream: TStream): Variant;

    class procedure WSL(Stream : TStream; const List: TStrings);
    class procedure RSL(Stream : TStream; List: TStrings);

    { math }
    class function RoundToTwo(D: Double): Double;
    class function RoundToCustom(Number: Double; Digits: Integer = 2): Double;
    class function RoundTo(Number: Double; Digits: Integer = 2): Double;
    class function IsWholeNumber(const D: Double): boolean;
    class function TwoComplement(Number: integer): integer;
    class function Percent(const Numerator, Denominator: Double ): Double;
    class function PercentOf(const Value, aPercent: Double): Double;
    class function Average(const Numerator, Denominator: Double ): Double;
    class function fmod(X, Y: Double): Double;

    class function Min(const A, B: Integer): Integer;  overload;
    class function Max(const A, B: Integer): Integer;  overload;
    class function Min(const A, B: Double): Double;    overload;
    class function Max(const A, B: Double): Double;    overload;
    {
    class function Min(const A, B: Extended): Extended;overload;
    class function Max(const A, B: Extended): Extended;overload;
    }

    { miscs }
    class function CreateSyncObject: ISyncObject;
    class function InMainThread(): Boolean;
    class procedure ProcessMessages();
    class procedure ClearObjectList(List: TList);

    { properties }
    class property InvariantFormatSettings    : TFormatSettings read FInvariantFormatSettings;


    { paths and folders }
    class property AppFolder                  : string read FAppFolder;
    class property AppDataFolder              : string read GetAppDataFolder;

    class property AppPath                    : string read FAppPath;
    class property AppExeName                 : string read FAppExeName;

    class property ProcessMessagesMethod      : TProcedureMethod read FProcessMessagesMethod write FProcessMessagesMethod;

  end;


  { TGenList }
  generic TGenList<T> = class(TPersistent)     // generic TGenList<T: TPersistent> = class(TPersistent)
  private
    FItems: array of T;   // https://lists.freepascal.org/pipermail/fpc-pascal/2018-May/053892.html
    FCount: SizeInt;
    function GetCount: Integer;
    function GetItem(Index: SizeInt): T;
    function GetItemList: specialize TArray<T>;
    procedure SetItem(Index: SizeInt; Item: T);
  protected
    function AdjustCapacityForItem(): SizeInt;  virtual;
    function AdjustCapacityForRange(RangeCount: SizeInt): SizeInt; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Clear(); virtual;

    function  Add(Item: T): Integer; virtual;
    procedure Insert(Index: Integer; Item: T); virtual;
    procedure Remove(Item: T); virtual;
    procedure RemoveAt(Index: Integer); virtual;

    procedure AddRange(constref Range: array of T); virtual;
    procedure InsertRange(Index: SizeInt; constref Range: array of T); virtual;

    function Contains(Item: T): Boolean; virtual;
    function IndexOf(Item: T): Integer; virtual;

    property Count: Integer read GetCount;

    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  //published
  //  property ItemList: specialize TArray<T> read GetItemList write FItemList;
  end;


implementation

uses
   laz2_XMLRead
  ,laz2_XMLWrite
  ,jsonscanner
  ,fpjson
  //,jsonparser

  ,XMLDatapacketReader

  ;

type

{ _TSyncObject }

_TSyncObject = class(TInterfacedObject, ISyncObject)
private
  FLock   : SyncObjs.TCriticalSection;
public
  constructor Create;
  destructor Destroy; override;

  procedure Lock;
  procedure UnLock;
end;

{ _TSyncObject }

constructor _TSyncObject.Create;
begin
  FLock := SyncObjs.TCriticalSection.Create();
end;

destructor _TSyncObject.Destroy;
begin
  FLock.Free();
  inherited Destroy;
end;

procedure _TSyncObject.Lock;
begin
  FLock.Enter();
end;

procedure _TSyncObject.UnLock;
begin
  FLock.Leave();
end;



{ TSafeObjectList }

constructor TSafeObjectList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;

  FLock := SyncObjs.TCriticalSection.Create();
  FList := TList.Create();
end;

destructor TSafeObjectList.Destroy;
begin
  Clear();
  FList.Free();
  FLock.Free;
  inherited Destroy;
end;

procedure TSafeObjectList.Lock;
begin
  FLock.Enter();
end;

procedure TSafeObjectList.UnLock;
begin
   FLock.Leave();
end;

function TSafeObjectList.GetIsEmpty: Boolean;
begin
  Lock();
  try
    Result := FList.Count = 0;
  finally
    UnLock();
  end;
end;

function TSafeObjectList.GetItem(Index: Integer): TObject;
begin
  Lock();
  try
    Result := TObject(FList[Index]);
  finally
    UnLock();
  end;
end;

function TSafeObjectList.GetCount: Integer;
begin
  Lock();
  try
    Result := FList.Count;
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Add(Instance: TObject);
begin
  Lock();
  try
    FList.Add(Instance);
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Insert(Index: Integer; Instance: TObject);
begin
  Lock();
  try
    FList.Insert(Index, Instance);
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Remove(Instance: TObject);
begin
  Lock();
  try
    FList.Remove(Instance);
    if FOwnsObjects then
       Instance.Free();
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.RemoveAt(Index: Integer);
var
  Instance: TObject;
begin
  Lock();
  try
    Instance := TObject(FList[Index]);
    Remove(Instance);
  finally
    UnLock();
  end;
end;

function TSafeObjectList.Contains(Instance: TObject): Boolean;
begin
  Lock();
  try
    Result := FList.IndexOf(Instance) <> -1;
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Clear();
begin
  Lock();
  try
    if FOwnsObjects then
      Sys.ClearObjectList(FList);
    FList.Clear();
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Push(Instance: TObject);
begin
  Lock();
  try
    FList.Add(Instance);
  finally
    UnLock();
  end;
end;

function TSafeObjectList.Pop(): TObject;
begin
  Lock();
  try
    if FList.Count > 0 then
    begin
      Result := TObject(FList.First);
      FList.Delete(0);
    end
    else
      Result := nil;
  finally
    UnLock();
  end;
end;

procedure TSafeObjectList.Sort(Compare: TListSortCompare);
begin
  Lock();
  try
    FList.Sort(Compare);
  finally
    UnLock();
  end;
end;




{ TKeyValue }

constructor TKeyValue.Create(AKey: string; AValue: Variant);
begin
  Self.Key := AKey;
  Self.Value := AValue;
end;

{ TVariantDictionary }

constructor TVariantDictionary.Create();
begin
  inherited Create();
  FList := TList.Create();
end;

destructor TVariantDictionary.Destroy;
begin
  Clear();
  FList.Free();
  inherited Destroy;
end;

procedure TVariantDictionary.Add(const Key: string; const Value: Variant);
begin
  Self.Item[Key] := Value;
end;

function TVariantDictionary.Remove(const Key: string): Boolean;
var
  Index: Integer;
  Pair : TKeyValue;
begin
  Result := False;
  Index := IndexOfKey(Key);
  if Index <> -1 then
  begin
    Pair := TKeyValue(FList[Index]);
    Pair.Free();
    FList.Delete(Index);
    Result := True;
  end;
end;

procedure TVariantDictionary.Clear();
begin
  while (FList.Count > 0) do
  begin
    try
      TObject(FList[FList.Count - 1]).Free;
    except
    end;
    FList.Delete(FList.Count - 1);
  end;

  FList.Clear();
end;

function TVariantDictionary.GetValue(const Key: string): Variant;
var
  i   : Integer;
  Pair: TKeyValue;
begin
  Pair := ItemByKey(Key);
  if Assigned(Pair) then
     Exit(Pair.Value);

  Result := Null;
end;

function TVariantDictionary.GetKeys: TArrayOfString;
var
  i   : Integer;
  Pair: TKeyValue;
begin
  SetLength(Result, FList.Count);

  for i := 0 to FList.Count do
  begin
    Pair := TKeyValue(FList[i]);
    Result[i] := Pair.Key;
  end;
end;

function TVariantDictionary.GetValues: TArrayOfVariant;
var
  i   : Integer;
  Pair: TKeyValue;
begin
  SetLength(Result, FList.Count);

  for i := 0 to FList.Count do
  begin
    Pair := TKeyValue(FList[i]);
    Result[i] := Pair.Value;
  end;
end;

function TVariantDictionary.GetCount: Integer;
begin
  Result := FList.Count;
end;



procedure TVariantDictionary.SetValue(const Key: string; Value: Variant);
var
  Pair: TKeyValue;
begin
  Pair := ItemByKey(Key);
  if not Assigned(Pair) then
  begin
    Pair := TKeyValue.Create(Key, Value);
    FList.Add(Pair);
  end;

  Pair.Value:= Value;
end;

function TVariantDictionary.ContainsValue(const Value: Variant): Boolean;
begin
  Result := IndexOfValue(Value) <> -1;
end;

function TVariantDictionary.IndexOfValue(const Value: Variant): Integer;
var
  i : Integer;
  Pair: TKeyValue;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Pair := TKeyValue(FList[i]);
    if VarSameValue(Value, Pair.Value) then
       Exit(i);
  end;

  Exit(-1);

end;

function TVariantDictionary.GetEnumerator(): TVariantDictionaryEnumerator;
begin
  Result := TVariantDictionaryEnumerator.Create();
  Result.FDictionary := Self;
end;

function TVariantDictionary.IndexOfKey(const Key: string): Integer;
var
  i : Integer;
  Pair: TKeyValue;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Pair := TKeyValue(FList[i]);
    if AnsiCompareText(Key, Pair.Key) = 0 then
       Exit(i);
  end;

  Exit(-1);
end;

function TVariantDictionary.ItemByKey(const Key: string): TKeyValue;
var
  i : Integer;
  Pair: TKeyValue;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Pair := TKeyValue(FList[i]);
    if AnsiCompareText(Key, Pair.Key) = 0 then
       Exit(Pair);
  end;

  Exit(nil);
end;

function TVariantDictionary.ContainsKey(const Key: string): Boolean;
begin
  Result := IndexOfKey(Key) <> -1;
end;


{ TVariantDictionaryEnumerator }
constructor TVariantDictionaryEnumerator.Create();
begin
  inherited Create();
  FPosition := -1;
end;

function TVariantDictionaryEnumerator.GetCurrent: TKeyValue;
begin
   Result := TKeyValue(FDictionary.FList[FPosition]);
end;

procedure TVariantDictionaryEnumerator.Reset;
begin
  FPosition := -1;
end;

function TVariantDictionaryEnumerator.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FDictionary.FList.Count;
end;



{ TWriteLineFile }
constructor TWriteLineFile.Create(FilePath: string; aColumnLine: string);
begin
  inherited Create();
  FIsClosed := True;

  FilePath := Trim(FilePath);
  if Length(FilePath) = 0 then
  begin
    FDefaultFileName := ExtractFileName(ParamStr(0));
    FDefaultFileName := ChangeFileExt(FDefaultFileName, '.log');
    FFolder          := ExtractFilePath(ParamStr(0));
    FFolder          := ConcatPaths([FFolder, 'Logs']);
  end else begin
    FDefaultFileName := ExtractFileName(FilePath);
    FFolder          := ExtractFilePath(FilePath);

    if Length(FFolder) = 0 then
      FFolder := ExtractFilePath(ParamStr(0));
  end;

  FColumnLine := Trim(aColumnLine);

  // create the first file
  BeginFile();
end;

destructor TWriteLineFile.Destroy;
begin
  CloseFile();
  inherited Destroy;
end;

function TWriteLineFile.GetRetainSizeKiloBytes: SizeInt;
begin
  if FRetainSizeKiloBytes >= 512 then
    Result := FRetainSizeKiloBytes
  else
    Result := 512;
end;

procedure TWriteLineFile.BeginFile();
var
  FilePath     : string;
begin
  if not DirectoryExists(FFolder) then
    CreateDir(FFolder);

  FLastFileName  := FormatDateTime('yyyy-mm-dd_hh_nn_ss__zzz_', NowUTC()) + FDefaultFileName;
  FilePath       := ConcatPaths([FFolder, FLastFileName]);

  CreateFile(FilePath);
end;

procedure TWriteLineFile.CreateFile(FilePath: string);
var
  Exists: Boolean;
begin
  CloseFile();
  Exists := FileExists(FilePath);
  Assign(F, FilePath);

  if Exists then
    Append(F)         // to append text to an existing text file
  else begin
    {$I-}             // without this, if rewrite fails then a runtime error will be generated
    Rewrite(F);       // to create a text file and write to it. If the file exists, it is truncated to zero length.
    {$I+}
  end;

  if IOResult <> 0 then
    raise Exception.CreateFmt('Cannot create or open a log file: %s', [FilePath]);

  FIsClosed := False;

  if Length(FColumnLine) > 0 then
     WriteLine(FColumnLine);
end;

procedure TWriteLineFile.CloseFile();
begin
  if not FIsClosed then
  begin
    Close(F);
    FIsClosed := True;
    FSize    := 0;
  end;
end;

procedure TWriteLineFile.WriteLine(Line: string);
begin
  Line := Trim(Line);

  if Length(Line) > 0 then
  begin
    if (FSize > (1024 * RetainSizeKiloBytes)) then
       BeginFile();

    System.WriteLn(F, Line);
    FSize := FSize + Length(Line);
    Flush(F);
  end;

end;

procedure TWriteLineFile.DeleteFilesOlderThan(Days: Integer);
var
  LastFilePath : string;
  FilePath     : string;

  FileList     : TStringList;
  i            : Integer;

  Age          : LongInt;

  FileDT       : TDateTime;
  NowDT        : TDateTime;

  DaysDiff     : Integer;
begin

  LastFilePath := ConcatPaths([Folder, LastFileName]);

  FileList := FindAllFiles(Folder, '*.*',  False);
  try
    i := FileList.IndexOf(LastFilePath);
    if i <> -1 then
       FileList.Delete(i);

    NowDT := Now();
    for i := 0 to FileList.Count - 1 do
    begin
      FilePath := FileList[i];

      Age := FileAge(FilePath);
      if Age <> -1 then
      begin
        FileDT   := FileDateToDateTime(Age);
        DaysDiff := DaysBetween(NowDT, FileDT);
        if DaysDiff > Days then
        try
          DeleteFile(FilePath);
        except
        end;
      end;
    end;
  finally
    FileList.Free();
  end;
end;




{ TBufTable }

constructor TBufTable.Create(AOwner: TComponent);
begin
  inherited;
  FFixedSource := TDataSource.Create(Self);
  FFixedSource.Dataset := Self;
end;

procedure TBufTable.SaveToFileXml(FilePath: string);
begin
  SaveToFile(FilePath, dfXMLUTF8);
end;

procedure TBufTable.LoadBlobIntoBuffer(FieldDef: TFieldDef;  ABlobBuf: PBufBlobField);
begin
  if Assigned(FieldDef) and Assigned(ABlobBuf) then
  begin
    //  do nothing. The LoadBlobIntoBuffer() is called by the GetNextPacket() only.
  end;
end;

procedure TBufTable.SetActive(Value: Boolean);
begin
  if (Value and (Fields.Count = 0) and (FieldDefs.Count > 0)) then
     CreateDataset();

  inherited SetActive(Value);
end;




{ Reflection }
class function Reflection.IsSameType(Info1, Info2: PTypeInfo): Boolean;
begin
  if Info1 = Info2 then
    Result := True
  else
    Result := (Info1^.Kind = Info2^.Kind) and SameStr(Info1^.Name, Info2^.Name) and SameStr(UnitNameOf(Info1), UnitNameOf(Info2));
end;

class function Reflection.IsSubRange(Info: PTypeInfo): Boolean;
var
  Data : PTypeData;
begin
  Data   := GetTypeData(Info);

  Result := False;
  case Info^.Kind of
    tkInteger      : Result := (Data^.MinValue > Low(Integer)) or (Data^.MaxValue < High(Integer));
    tkChar         : Result := (Data^.MinValue > Ord(Low(Char))) or (Data^.MaxValue < Ord(High(Char)));
    tkEnumeration  : Result := (Data^.BaseType <> Info);
    tkWChar        : Result := (Data^.MinValue > Ord(Low(WChar))) or (Data^.MaxValue < Ord(High(WChar)));
  end;
end;

class function Reflection.IsOrdinal(Info: PTypeInfo): Boolean;
begin
  Result := Info^.Kind in [tkChar, tkBool, tkWChar, tkSet, tkEnumeration, tkInteger, tkInt64, tkQword];
end;
(*----------------------------------------------------------------------------
  Returns Truf if the Instance is a descendant of the class ClassName.
  If InfoList <> nil then it is filled with the names of all
  the ancestor classes.
  WARNING: This function works by comparing the names (strings)
  of classes, NOT the pointers of the classes (class references)
----------------------------------------------------------------------------*)
class function Reflection.InheritsFrom(Instance: TObject; const aClassName: string; InfoList: TStrings): Boolean;
var
  C          : TClass;
  List       : TStringList;
begin
  Result := False;
  if not Assigned(Instance) then Exit; //==>

  List := TStringList.Create;
  try
    List.CaseSensitive := False;
    List.Insert(0, Instance.ClassName);

    if Sys.IsSameText(ClassName, Instance.ClassName) then
    begin
      Result := True;
      Exit; //==>
    end;

    C := Instance.ClassParent;
    while Assigned(C) do
    begin
      List.Insert(0, C.ClassName);

      if Sys.IsSameText(ClassName, C.ClassName) then
      begin
        Result := True;
        Exit; //==>
      end;

      C := C.ClassParent;
    end;

    Result := List.IndexOf(aClassName) <> -1;

  finally
    if Assigned(InfoList) then
      InfoList.Assign(List);
    List.Free;
  end;

end;

class function Reflection.UnitNameOf(Info: PTypeInfo): string;
{---------------------------------------------}
function  EnumTypeUnitName(Info: PTypeInfo): string;
var
  I        : Integer;
  P        : PShortString;
  pData    : PTypeData;

begin
  pData := TypInfo.GetTypeData(Info);

  if (pData^.BaseType <> Info) and Assigned(pData^.BaseType) then
    pData := GetTypeData(pData^.BaseType);

  I := pData^.MaxValue - pData^.MinValue;
  P := @pData^.NameList;      // for enum types, the last string in the NameList is the name of the unit
  while I >= 0 do
  begin
    Inc(PChar(P), Length(P^) + 1);
    Dec(I);
  end;
  Result := P^;

end;
{---------------------------------------------}
begin
  Result := '';
  try
    case Info^.Kind of
      tkEnumeration    : Result := EnumTypeUnitName(Info);
      tkClass          : Result := GetTypeData(Info)^.UnitName;
      tkInterface      : Result := GetTypeData(Info)^.IntfUnit;
      tkDynArray       : Result := GetTypeData(Info)^.DynUnitName;
    end;
  except
  end;
end;

class function Reflection.BaseOf(Info: PTypeInfo): PTypeInfo;
var
  Data : PTypeData;
begin
  Result := nil;

  if Assigned(Info) and (Info^.Kind in [tkEnumeration, tkSet, tkClass, tkInterface]) then
  begin
    Data := GetTypeData(Info);
    case Info^.Kind of
      tkEnumeration : if Data^.BaseType = Info then
                        Result := Info
                      else
                        Result := Data^.BaseType;
      tkSet         : Result := Data^.CompType;
      tkClass       : if (Data^.ParentInfo <> nil)  then
                        Result := Data^.ParentInfo;
      tkInterface   : if (Data^.IntfParent <> nil)  then
                        Result := Data^.IntfParent;

    end;
  end;

end;

class function Reflection.TypeSizeOf(Info: PTypeInfo): Integer;
var
  Data: PTypeData;
begin
  Result := System.SizeOf(Integer);
  Data := GetTypeData(Info);
  case Info^.Kind of
    tkSet         ,
    tkEnumeration ,
    tkInteger     : case Data^.OrdType of
                      otSByte ,
                      otUByte : Result := System.SizeOf(Byte);
                      otSWord ,
                      otUWord : Result := System.SizeOf(Word);
                      otSLong ,
                      otULong : ;
                    end;
    tkFloat       : case Data^.FloatType of
                      ftSingle   : Result := System.SizeOf(Single);
                      ftDouble   : Result := System.SizeOf(Double);
                      ftComp     : Result := System.SizeOf(Comp);
                      ftCurr     : Result := System.SizeOf(Currency);
                      ftExtended : Result := System.SizeOf(Extended);
                    end;

    TypInfo.tkString      : Result := Data^.MaxLength;
    tkChar        : Result := System.SizeOf(Char);
    tkLString     : Result := System.SizeOf(AnsiString);
    tkWChar       : Result := System.SizeOf(WideChar);
    tkInt64       : Result := System.SizeOf(Int64);
    tkVariant     : Result := System.SizeOf(TVarData);
  end;

end;

class function Reflection.HasProperty(Instance: TObject; const PropertyName: string): Boolean;
begin
  Result := GetPropInfo(Instance.ClassInfo, PropertyName) <> nil;
end;

(*--------------------------------------------------------------------------------*)
class function Reflection.SetValueToInt(Info: PTypeInfo; const SetVar): Integer;
var
  BitShift   : Integer;
  TmpInt64   : Int64;
  EnumMin    : Integer;
  EnumMax    : Integer;
  ResBytes   : Integer;
  CompType   : PTypeInfo;
begin
  Result := 0;
  TmpInt64 := 0;
  CompType := GetTypeData(Info)^.CompType;
  EnumMin := GetTypeData(CompType)^.MinValue;
  EnumMax := GetTypeData(CompType)^.MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if (EnumMax - EnumMin) > 32 then
    raise Exception.CreateFmt( 'Value out of range (%d) bits', [EnumMax - EnumMin]);
  BitShift := EnumMin mod 8;
  Move(SetVar, TmpInt64, ResBytes + 1);
  TmpInt64 := TmpInt64 shr BitShift;
  Move(TmpInt64, Result, ResBytes);
end;
(*--------------------------------------------------------------------------------*)
class procedure Reflection.IntToSetValue(Info: PTypeInfo; var SetVar; const Value: Integer);
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
begin
  CompType := GetTypeData(Info)^.CompType;
  EnumMin := GetTypeData(CompType)^.MinValue;
  EnumMax := GetTypeData(CompType)^.MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  BitShift := EnumMin mod 8;
  TmpInt64 := Longword(Value) shl BitShift;
  Move(TmpInt64, SetVar, ResBytes);
end;
(*----------------------------------------------------------------------------*)
class function Reflection.SetValueToStr(Info: PTypeInfo; const SetVar; const Brackets: Boolean): string;
var
  Value     : TIntegerSet;
  EnumValue : 0..System.SizeOf(Integer) * 8 - 1;
begin
  Assert(Info^.Kind in [tkEnumeration, tkSet], 'TypeKind is not in [tkEnumeration, tkSet]');

  Value := TIntegerSet(SetValueToInt(Info, SetVar));

  if Info^.Kind = tkSet then
    Info := GetTypeData(Info)^.CompType;

  Result := '';
  for EnumValue := GetTypeData(Info)^.MinValue to
    GetTypeData(Info)^.MaxValue do
    if EnumValue in Value then
      if Result = '' then
        Result := GetEnumName(Info, EnumValue)
      else
        Result := Result + ', ' + GetEnumName(Info, EnumValue);

  if Brackets then
    Result := '[' + Result + ']';
end;
(*----------------------------------------------------------------------------*)
class procedure Reflection.StrToSetValue(Info: PTypeInfo; var SetVar; const Value: string);
const
  SInvalidSetStr = '''%s'' is not a valid set string';
var
  Result    : TIntegerSet;
  EnumValue : 0..System.SizeOf(Integer) * 8 - 1;
  S         : string;
  Strings   : TStrings;
  i         : Integer;
  SetInfo   : PTypeInfo;
begin
  Assert(Info^.Kind in [tkEnumeration, tkSet], 'TypeKind is not in [tkEnumeration, tkSet]');

  SetInfo := Info;


  if Info^.Kind = tkSet then
    Info := GetTypeData(Info)^.CompType;

  Result := [];
  S := Trim(Value);
  if (S[1] = '[') and (S[Length(S)] = ']') then
  begin
    S := Copy(S, 2, Length(S) - 2);
    Strings := TStringList.Create;
    try
      Strings.CommaText := S;
      for i := 0 to Strings.Count - 1 do
      begin
        EnumValue := GetEnumValue(Info, Trim(Strings[i]));
        if (EnumValue < GetTypeData(Info)^.MinValue) or
          (EnumValue > GetTypeData(Info)^.MaxValue) then
          raise EConvertError.Create(Format(SInvalidSetStr, [Value]));

        Include(TIntegerSet(Result), EnumValue);
      end;
    finally
      Strings.Free;
    end;
  end;

  IntToSetValue(SetInfo, SetVar, Integer(Result));

end;
(*----------------------------------------------------------------------------*)
class function Reflection.SetLiteralsToStr(Info: PTypeInfo): string;
var
  i   : Integer;
  List : TStringList;
begin
  List := TStringList.Create;
  try
    with GetTypeData(GetTypeData(Info)^.CompType)^ do
      for I := MinValue to MaxValue do
        List.Add(GetEnumName(GetTypeData(Info)^.CompType, i));

    Result := List.CommaText;
  finally
    List.Free;
  end;

end;
(*----------------------------------------------------------------------------*)
class procedure Reflection.EnumTypeToList(Info: PTypeInfo; List: TStrings; PrefixCut: Integer);
var
 pTD : PTypeData;
 i   : integer;
 S   : string;
begin
  pTD := GetTypeData(Info);
  S := '';
  for i := pTD^.MinValue  to pTD^.MaxValue do
  begin
    S := GetEnumName(Info, i);
    if  (PrefixCut <> 0)  then
      S := Copy(S, PrefixCut + 1, Length(S) - PrefixCut);
    List.Add(S);
  end;

end;
(*----------------------------------------------------------------------------*)
class function Reflection.EnumValueToStr(Info: PTypeInfo; Value: Integer; PrefixCut: Integer): string;
begin
  Result := GetEnumName(Info, Value);
  if  (PrefixCut <> 0) then
    Result := Copy(Result, PrefixCut + 1, Length(Result) - PrefixCut);
end;
(*----------------------------------------------------------------------------*)
class function Reflection.EnumLiteralsMinIndex(Info: PTypeInfo): Integer;
begin
  if (Info = TypeInfo(System.Boolean))  then
    Result := 0
  else if (Info = TypeInfo(System.ByteBool)) or (Info = TypeInfo(System.WordBool)) or (Info = TypeInfo(System.LongBool)) then
    Result := -1
  else
    Result := GetTypeData(Info)^.MinValue
end;
(*----------------------------------------------------------------------------*)
class function Reflection.EnumLiteralsMaxIndex(Info: PTypeInfo): Integer;
begin
  if (Info = TypeInfo(System.Boolean))  then
    Result := 1
  else if (Info = TypeInfo(System.ByteBool)) or (Info = TypeInfo(System.WordBool)) or (Info = TypeInfo(System.LongBool)) then
    Result := 1
  else
    Result := GetTypeData(Info)^.MaxValue
end;
(*----------------------------------------------------------------------------
Boolean	                  ByteBool, WordBool, LongBool
False < True	            False <> True
Ord(False) = 0	          Ord(False) = 0
Ord(True) = 1	            Ord(True) <> 0
Succ(False) = True	      Succ(False) = True
Pred(True) = False	      Pred(False) = True
 ----------------------------------------------------------------------------*)
class function Reflection.EnumLiteralsToStr(Info: PTypeInfo): string;
var
 pData : PTypeData;
 i     : Integer;
begin
  if (Info = TypeInfo(System.Boolean))  then
    Result := 'False, True'
  else if (Info = TypeInfo(System.ByteBool)) or (Info = TypeInfo(System.WordBool)) or (Info = TypeInfo(System.LongBool)) then
    Result := 'True, False, True'
  else begin
    pData := GetTypeData(Info);

    Result := '';
    for i := pData^.MinValue  to pData^.MaxValue do
      Result := Result + GetEnumName(Info, i) + ', ';

    if Length(Result) > 2 then
      SetLength(Result, Length(Result) - 2);
  end;

end;
(*----------------------------------------------------------------------------*)
class function Reflection.EnumLiteralByIndex(Info: PTypeInfo; Index: Integer): string;
var
  pData : PTypeData;
  P     : PShortString;
begin
  Result := '';

  if (Info = TypeInfo(System.Boolean))  then
  begin
    case Index of
      0 : Result := 'False';
      1 : Result := 'True';
    end;
  end else if (Info = TypeInfo(System.ByteBool)) or (Info = TypeInfo(System.WordBool)) or (Info = TypeInfo(System.LongBool)) then
  begin
    case Index of
      -1 : Result := 'True';
       0 : Result := 'False';
       1 : Result := 'True';
    end;
  end else begin
    pData := GetTypeData(Info);

    if (pData^.BaseType <> Info) and Assigned(pData^.BaseType) then
      pData := GetTypeData(pData^.BaseType);

    P := @pData^.NameList;
    while Index <> 0 do
    begin
      Inc(PChar(P), Length(P^) + 1);
      Dec(Index);
    end;
    Result := P^;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Reflection.EnumIndexOfLiteral(Info: PTypeInfo; const Name: string): Integer;
var
  Data : PTypeData;
begin
  Data := GetTypeData(Info);

  Result := Data^.MaxValue;
  while (Result >= Data^.MinValue) and not Sys.IsSameText(Name, EnumLiteralByIndex(Info, Result)) do
    Dec(Result);
  if Result < Data^.MinValue then
    Result := -1;
end;
(*----------------------------------------------------------------------------*)
class function Reflection.EventsEqual(A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;
(*----------------------------------------------------------------------------*)
class function Reflection.NotifyEventsEqual(A, B: TNotifyEvent): Boolean;
begin
  Result := EventsEqual(TMethod(A), TMethod(B));
end;
(*----------------------------------------------------------------------------*)
{ replaces the event of an object with the event of another object.
  Returns the TMethod of the old event }
class function Reflection.ReplaceEvent(TargetObject: TObject; TargetEventName: string; NewObject: TObject; NewEventName: string): TMethod;
var
  Method : TMethod;
begin
  Result.Code := nil; // pointer to method
  Result.Data := nil; // pointer to Self

  if not Assigned(TargetObject) then Exit; //==>
  if not Assigned(NewObject) then Exit; //==>

  Result := GetMethodProp(TargetObject, TargetEventName);

  Method.Code := NewObject.MethodAddress(NewEventName);
  Method.Data := NewObject;
  SetMethodProp(TargetObject, TargetEventName, Method);
end;
(*----------------------------------------------------------------------------
  Replaces the event of an object with the event of another object.
  Returns the TMethod of the old event.

  This function may called when two executables (exe and dll) are compiled
  with run-time packages.

 Example :
     var
       OldEvent : TMethod;
     begin
       OldEvent := ReplaceEvent2(Target, 'OnSomething', NewObject, @TNewObject.DoSomethingElse);
     end;

 The old event handler method which may be called as

     procedure TNewObject.DoSomethingElse;
     type
       TEvent = procedure of object;
     var
       Event : TEvent;
     begin

       // Process the event here and then call the old event

       TMethod(Event).Data := OLD_Event.Data;
       TMethod(Event).Code := OLD_Event.Code;
       if Assigned(Event) then
         Event;
     end;
-------------------------------------------------------------------------------*)
class function Reflection.ReplaceEvent2(TargetObject: TObject; TargetEventName: string; NewObject: TObject; NewEventMethod: Pointer): TMethod;
var
  Method   : TMethod;
  PropInfo : PPropInfo;
begin
  Result.Code := nil; // pointer to method
  Result.Data := nil; // pointer to Self

  if not Assigned(TargetObject) then Exit; //==>
  if not Assigned(NewObject) then Exit; //==>

  // if no event property exists, exit...
  PropInfo := GetPropInfo(TargetObject.ClassInfo, TargetEventName);
  if not Assigned(PropInfo) then Exit; //==>

  Result := TypInfo.GetMethodProp(TargetObject, PropInfo);

  Method.Code  := NewEventMethod;
  Method.Data  := NewObject;
  TypInfo.SetMethodProp(TargetObject, PropInfo, Method);
end;

class function Reflection.ComponentToString(Component: TComponent): string;
var
  MS     : TMemoryStream;
  SS     : TStringStream;
  Writer : TWriter;
begin
  MS := TMemoryStream.Create;
  SS := TStringStream.Create('');
  try
    Writer := TWriter.Create(MS, 4096);
    try
     Writer.IgnoreChildren := False;
     Writer.WriteRootComponent(Component);
    finally
      Writer.Free;
    end;
    MS.Position := 0;
    ObjectBinaryToText(MS, SS);
    SS.Position := 0;
    Result := SS.DataString;
  finally
    SS.Free;
    MS.Free
  end;
end;
(*----------------------------------------------------------------------------*)
class function Reflection.StringToComponent(Owner: TComponent; Parent: TComponent; Component: TComponent; Value: string): TComponent;
var
  SS     : TStringStream;
  MS     : TMemoryStream;
  Reader : TReader;
begin
  MS := TMemoryStream.Create;
  SS := TStringStream.Create(Value);
  try
    SS.Position := 0;
    ObjectTextToBinary(SS, MS);
    MS.Position := 0;
    Reader := TReader.Create(MS, 4096);
    try
     Reader.Root  := Parent;
     Reader.Owner := Owner;
     Result := Reader.ReadComponent(Component);
    finally
      Reader.Free;
    end;

  finally
    SS.Free;
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------
 Reads a text .dfm and creates the components of the Form
 ----------------------------------------------------------------------------*)
 class procedure Reflection.ReadFormTextResource(TextResource: string; Form: TComponent);
var
  SS     : TStringStream;
  MS     : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  SS := TStringStream.Create(TextResource);
  try
    SS.Position := 0;
    ObjectTextToBinary(SS, MS);
    MS.Position := 0;
    MS.ReadComponent(Form);
  finally
    SS.Free;
    MS.Free;
  end;
end;
class procedure Reflection.CreatePascalFiles(Form: TComponent; aUnitName: string = '');
const
  cUnit =
          'unit %s;                                                                         ' + #13 +
          '                                                                                 ' + #13 +
          'interface                                                                        ' + #13 +
          '                                                                                 ' + #13 +
          'uses                                                                             ' + #13 +
          '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,     ' + #13 +
          '  Dialogs;                                                                       ' + #13 +
          '                                                                                 ' + #13 +
          'type                                                                             ' + #13 +
          '  %s = class(%s)                                                                 ' + #13 +
          '%s                                                                               ' + #13 +
          '  end;                                                                           ' + #13 +
          '                                                                                 ' + #13 +
          'implementation                                                                   ' + #13 +
          '                                                                                 ' + #13 +
          '{$R *.dfm}                                                                       ' + #13 +
          '                                                                                 ' + #13 +
          'end.                                                                             ' + #13 +
          '';
var
 sDFM  : string;
 sUnit : string;
 List  : TStringList;
 i     : Integer;

begin
  if aUnitName = '' then
  begin
    aUnitName := Form.ClassName;
    Delete(aUnitName, 1, 1);
  end;

  aUnitName := ExtractFileName(aUnitName);
  aUnitName := ChangeFileExt(aUnitName, '');

  aUnitName := 'f_' + aUnitName;

  List := TStringList.Create;
  try
    for i := 0 to Form.ComponentCount - 1 do
      if Form.Components[i].Name <> '' then
        List.Add(Format('    %s: %s;', [Form.Components[i].Name, Form.Components[i].ClassName]));

    sDFM  := ComponentToString(Form);
    sUnit := Format(cUnit, [ aUnitName,
                             Form.ClassName,
                             Form.ClassParent.ClassName,
                             List.Text
                           ]);

    List.Text := sDFM;
    List.SaveToFile(aUnitName + '.dfm');
    List.Text := sUnit;
    List.SaveToFile(aUnitName + '.pas');

  finally
    List.Free;
  end;

end;





type
  { TJsonHelper }
  TJsonHelper = class
  protected
    procedure propertyError(Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData; Error : Exception; var doContinue : Boolean); virtual;
    procedure restoreProperty(Sender : TObject; AObject : TObject; Info : PPropInfo; AValue : TJSONData; var Handled : Boolean); virtual;
  end;

{ TJsonHelper }

procedure TJsonHelper.propertyError(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; Error: Exception; var doContinue: Boolean
  );
begin
end;

procedure TJsonHelper.restoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
begin
end;


{ Json }
class function Json.Serialize(Instance: TObject; Options: TJSONStreamOptions): string;
var
  Streamer  : TJSONStreamer;
  Data      : TJSONStringType;
  JsonData  : TJSONData;
begin
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options := Options;
  try
    Data := Streamer.ObjectToJSONString(Instance);
    JsonData := GetJSON(Data, true);
    try
      Result := JsonData.FormatJSON([], 2);
    finally
      JsonData.Free();
    end;
  finally
    FreeAndNil(Streamer);
  end;
end;

class procedure Json.Deserialize(Instance: TObject; JsonText: string; Options: TJSONDestreamOptions);
var
  DeStreamer: TJSONDeStreamer;
  Helper: TJsonHelper;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  Helper := TJsonHelper.Create();
  try
    DeStreamer.Options := Options;
    DeStreamer.OnPropertyError   := @Helper.propertyError;
    DeStreamer.OnRestoreProperty := @Helper.restoreProperty;
    DeStreamer.JSONToObject(JsonText, Instance);
  finally
    FreeAndNil(Helper);
    FreeAndNil(DeStreamer);
  end;
end;

class procedure Json.SaveToFile(FilePath: string; Instance: TObject);
var
  JsonText : string;
begin
  JsonText := Serialize(Instance);
  Sys.SaveToFile(FilePath, JsonText);
end;

class procedure Json.LoadFromFile(FilePath: string; Instance: TObject);
var
  JsonText : string;
begin
  if FileExists(FilePath) then
  begin
     JsonText := Sys.LoadFromFile(FilePath);
     Deserialize(Instance, JsonText);
  end;
end;
















{ Xml }

class function Xml.IsValidName(Name: string): Boolean;
var
  i : Integer;
begin
  Result := False;

  if Length(Name) = 0 then
    Exit; //==>

  if not (Sys.IsLetter(Name[1]) or (Name[1] = '_')) then
    Exit; //==>

  for i := 2 to Length(Name) do
    if (not Sys.IsLetter(Name[i])) or (not CharInSet(Name[i], ['.', '-', '_'])) then
      Exit; //==>

  Result := True;

end;

class function Xml.NormalizeTypeName(Name: string): string;
var
  i : Integer;
begin
  Result := '';

  if (Length(Name) > 0) then
  begin
    if Name[1] = 'T' then
      Delete(Name, 1, 1);

    for i := 1 to Length(Name) do
      if not CharInSet(Name[i], ['<', '>', '`', '+']) then     {, '0'..'9'}
        Result := Result + Name[i];
  end;
end;

class function Xml.VariantToXml(const Value: Variant): string;
begin
  if Sys.VarIsNull(Value) then
    Result := ''
  else begin
    case TVarData(Value).VType of
      varSmallint, varInteger, varByte:
        Result := IntToStr(Value);

      varSingle, varDouble, varCurrency:
        Result := SysUtils.FloatToStr(Value);

      varDate:
        Result := SysUtils.DateToStr(Value);

      varOleStr, varString, varUString, varVariant:
        Result := VarToStr(Value);

      varBoolean:
        if Value = True then Result := '1' else Result := '0';

      else
        Result := '';
    end;
  end;
end;

class function Xml.StrToXml(const S: string): string;
const
  SpecChars = ['<', '>', '"', '&', #10, #13];
var
  i: Integer;

  procedure ReplaceChars(var S: string; i: Integer);
  begin
    System.Insert('#' + IntToStr(Ord(S[i])) + ';', S, i + 1);
    S[i] := '&';
  end;

begin
  Result := S;
  for i := Length(S) downto 1 do
    if S[i] in SpecChars then
      ReplaceChars(Result, i);

end;

class function Xml.XmlToStr(Value: Variant): string;
var
  i, j, h, n: Integer;
  S : string;
begin

  if VarIsNull(Value) then
    S := ''
  else
    S := Value;

  Result := S;

  i := 1;
  n := Length(S);
  while i < n do
  begin
    if i + 3 <= n then
      if (Result[i] = '&') and (Result[i + 1] = '#') then
      begin
        j := i + 3;
        while Result[j] <> ';' do
          Inc(j);
        h := StrToInt(Copy(Result, i + 2, j - i - 2));
        Delete(Result, i, j - i);
        Result[i] := Chr(h);
        Dec(n, j - i);
      end;
    Inc(i);
  end;

end;

class function Xml.CreateDoc(RootName: string; Encoding: string): TXMLDocument;
begin

  Result := TXMLDocument.Create;

  if Encoding = '' then
    Encoding := EncodingUtf8;

  if RootName = '' then
    RootName := 'root';

  Result.AppendChild(Result.CreateElement(NormalizeTypeName(RootName)));

  Result.XMLVersion := '1.0';
  Result.Encoding   := Encoding;
end;

class function Xml.TextToDoc(const XmlText: string): TXMLDocument;
var
   SS: TStringStream;
begin
  if (Length(XmlText) > 0) then
  begin
    SS := TStringStream.Create(XMLText);
    try
      ReadXMLFile(Result, SS);
    finally
      SS.Free;
    end;
  end else begin
    Result := TXMLDocument.Create;
  end;
end;

class function Xml.DocToText(Doc: TXMLDocument): string;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    WriteXMLFile(Doc, SS);
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

class function Xml.StreamToDoc(Stream: TStream): TXMLDocument;
begin
  Stream.Position := 0;
  ReadXMLFile(Result, Stream);
end;

class function Xml.DocToStream(Doc: TXMLDocument): TStream;
begin
  Result := TMemoryStream.Create();
  WriteXMLFile(Doc, Result);
end;

class procedure Xml.SaveToFile(Doc: TXMLDocument; FilePath: string);
begin
  WriteXMLFile(Doc, FilePath);
end;

class function Xml.LoadFromFile(FilePath: string): TXMLDocument;
begin
  if (FileExists(FilePath)) then
  begin
    ReadXMLFile(Result, FilePath);
  end else
  begin
    Result := CreateDoc();
  end;
end;

class function Xml.AddNode(Parent: TDOMNode; ChildName: string; Value: Variant): TDOMElement;
var
  TextNode: TDOMText;
begin
  Result := Parent.OwnerDocument.CreateElement(ChildName);
  Parent.AppendChild(Result);
  if (not Sys.VarIsNull(Value)) then
  begin
    TextNode := Parent.OwnerDocument.CreateTextNode(VarToStr(Value));
    Result.AppendChild(TextNode);
  end;
end;

class function Xml.AddNode(Parent: TDOMNode; ChildName: string): TDOMElement;
begin
  Result := AddNode(Parent, ChildName, Variants.Null());
end;

class function Xml.AddCData(Parent: TDOMNode; ChildName: string; CDataText: string): TDOMElement;
begin
  Result := Parent.OwnerDocument.CreateElement(ChildName);
  Parent.AppendChild(Result);
  SetCData(Result, CDataText);
end;

class function Xml.GetNodeValue(const Node: TDOMNode; DefaultValue: string): string;
var
  ChildNode: TDOMNode;
begin
  Result := DefaultValue;

  if (Node is TDOMElement) then
  begin
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      if (ChildNode is TDOMText) then
      begin
        Result := ChildNode.NodeValue;
        Exit;
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

class procedure Xml.SetNodeValue(const Node: TDOMNode; Value: Variant);
var
  ChildNode: TDOMNode;
  TextNode: TDOMText;
  sValue: string;
begin
  if (Sys.VarIsNull(Value)) then
     sValue := ''
  else
    sValue := VarToStr(Value);

  if (Node is TDOMElement) then
  begin
    ChildNode := Node.FirstChild;

    while Assigned(ChildNode) do
    begin
      if (ChildNode is TDOMText) then
      begin
        ChildNode.NodeValue := sValue;
        Exit;
      end;
      ChildNode := ChildNode.NextSibling;
    end;

    TextNode := Node.OwnerDocument.CreateTextNode(sValue);
    Node.AppendChild(TextNode);

  end;

end;

class function Xml.GetChild(const Parent: TDOMNode; ChildName: string; DefaultValue: string): string;
var
  Node: TDOMNode;
begin
  Result := DefaultValue;
  Node := Parent.FindNode(ChildName);
  if (Assigned(Node)) then
    Result := GetNodeValue(Node, DefaultValue);
end;

class procedure Xml.SetChild(const Parent: TDOMNode; ChildName: string; Value: Variant);
var
  Node: TDOMNode;
begin
  Node := Parent.FindNode(ChildName);
  if (not Assigned(Node)) then
  begin
    Node := Parent.OwnerDocument.CreateElement(ChildName);
    Parent.AppendChild(Node);
  end;

  SetNodeValue(Node, Value);
end;

class function Xml.GetAttr(const Node: TDOMNode; AttrName: string; DefaultValue: string): string;
begin
  if (Node is TDOMElement) and (Assigned(TDOMElement(Node).GetAttributeNode(AttrName))) then
    Result := TDOMElement(Node)[AttrName]
  else
    Result := DefaultValue;
end;

class procedure Xml.SetAttr(const Node: TDOMNode; AttrName: string; Value: Variant);
var
  sValue: string;
begin
   if (Node is TDOMElement) then
   begin
     if (Sys.VarIsNull(Value)) then
        sValue := ''
     else
       sValue := VarToStr(Value);
       TDOMElement(Node)[AttrName] := sValue;
   end;
end;

class function Xml.GetCData(const Node: TDOMNode; DefaultValue: string): string;
var
  ChildNode: TDOMNode;
begin
  Result := DefaultValue;

  if (Node is TDOMElement) then
  begin
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      if (ChildNode is TDOMCDATASection) then
      begin
        Result := ChildNode.NodeValue;
        Exit;
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;

end;

class procedure Xml.SetCData(const Node: TDOMNode; Value: string);
var
  ChildNode: TDOMNode;
  CDataNode: TDOMCDATASection;
begin
   if (Node is TDOMElement) then
   begin
     ChildNode := Node.FirstChild;

     while Assigned(ChildNode) do
     begin
       if (ChildNode is TDOMCDATASection) then
       begin
         ChildNode.NodeValue := Value;
         Exit;
       end;
       ChildNode := ChildNode.NextSibling;
     end;

     CDataNode := Node.OwnerDocument.CreateCDATASection(Value);
     Node.AppendChild(CDataNode);

   end;
end;

class function Xml.GetCData(const Parent: TDOMNode; ChildName: string; DefaultValue: string): string;
begin
  Result := GetCData(Parent.FindNode(ChildName), DefaultValue);
end;

class procedure Xml.SetCData(const Parent: TDOMNode; ChildName: string; Value: string);
var
  Node: TDOMNode;
begin
  Node := Parent.FindNode(ChildName);
  if (not Assigned(Node)) then
  begin
    Node := Parent.OwnerDocument.CreateElement(ChildName);
    Parent.AppendChild(Node);
  end;
  SetCData(Node, Value);
end;

class procedure Xml.DatasetSchemaToXml(Table: TDataset; TopNode: TDOMElement);
var
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  i         : Integer;
  Field     : TField;
begin


  if Reflection.HasProperty(Table, 'TableName') then
     Xml.SetAttr(TopNode, 'TableName', Xml.StrToXml(GetStrProp(Table, 'TableName')));

  Node := Xml.AddNode(TopNode, 'schema');


  for i := 0 to Table.FieldCount - 1 do
  begin
    Field   := Table.Fields[i];
    if Field.FieldKind = fkData then
    begin
      SubNode := Xml.AddNode(Node, 'c' + IntToStr(i));
      SubNode['name'] := Field.FieldName;

      case Field.DataType of
        ftString        ,
        ftFixedChar     ,
        ftGuid          ,
        ftWideString    ,
        ftFixedWideChar : begin
                            SubNode['type']  := 'string';
                            SubNode['width'] := Field.Size.ToString();
                          end;
        ftSmallint   ,
        ftInteger    ,
        ftWord       ,
        ftLargeint   : SubNode['type'] := 'integer';
        ftAutoInc    : SubNode['type'] := 'autoinc';

        ftBoolean    : SubNode['type'] := 'boolean';

        ftFloat      ,
        ftBCD        ,
        ftFMTBcd     : SubNode['type'] := 'float';
        ftCurrency   : SubNode['type'] := 'money';

        ftDate       : SubNode['type'] := 'date';
        ftTime       : SubNode['type'] := 'time';
        ftDateTime   : SubNode['type'] := 'datetime';
        ftTimeStamp  : SubNode['type'] := 'datetime';

        ftMemo       ,
        ftWideMemo   ,
        ftFmtMemo    ,
        ftOraClob    : SubNode['type'] := 'memo';

        ftGraphic    : SubNode['type'] := 'graphic';

        ftOraBlob    ,
        ftBlob       : SubNode['type'] := 'blob';
        else           raise Exception.Create('Field data type not supported');
      end;

      if Field.Required then
        SubNode['required'] := 'true';

      if Field.ReadOnly then
        SubNode['readonly'] := 'true';
    end;

  end;


end;

class procedure Xml.XmlToDatasetSchema(Table: TDataset; TopNode: TDOMElement);
  {-------------------------------------------------------------------}
  function AsVariant(const Value: Variant; Default: Variant): Variant;
  begin
    Result := Value;
    if VarIsNull(Result) then
      Result := Default;
  end;
  {-------------------------------------------------------------------}
  function GetFieldType(const sType: string): TFieldType;
  var
    FieldType : TFieldType;
  begin
     FieldType := ftString;

          if sType = 'string'     then FieldType := ftString
     else if sType = 'integer'    then FieldType := ftInteger
     else if sType = 'autoinc'    then FieldType := ftAutoInc
     else if sType = 'boolean'    then FieldType := ftBoolean
     else if sType = 'float'      then FieldType := ftFloat
     else if sType = 'money'      then FieldType := ftCurrency
     else if sType = 'date'       then FieldType := ftDate
     else if sType = 'time'       then FieldType := ftTime
     else if sType = 'datetime'   then FieldType := ftDateTime
     else if sType = 'memo'       then FieldType := ftMemo
     else if sType = 'graphic'    then FieldType := ftGraphic
     else if sType = 'blob'       then FieldType := ftBlob
     ;

     Result := FieldType;
  end;
  {-------------------------------------------------------------------}
var
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  Nodes     : TDOMNodeList;
  i         : Integer;
  FieldDef  : TFieldDef;
begin

  if Reflection.HasProperty(Table, 'TableName') and (GetStrProp(Table, 'TableName') = '') and (Xml.GetAttr(TopNode, 'TableName', '') <> '') then
     SetStrProp(Table, 'TableName', (Xml.GetAttr(TopNode, 'TableName', '')));


  Node  := TopNode.FindNode('schema') as TDOMElement;
  Nodes := Node.ChildNodes;

  for i := 0 to Nodes.Count - 1 do
  begin
    SubNode := Nodes[i] as TDOMElement;

    FieldDef               := Table.FieldDefs.AddFieldDef();
    FieldDef.Name          := SubNode['name'];
    FieldDef.DataType      := GetFieldType(SubNode['type']);
    FieldDef.Size          := AsVariant(SubNode['width'], 0);
    FieldDef.Required      := AsVariant(SubNode['required'], False);
    if  AsVariant(SubNode['readonly'], False) then
      FieldDef.Attributes := FieldDef.Attributes + [faReadonly];
  end;

end;

class procedure Xml.DatasetDataToXml(Table: TDataset; TopNode: TDOMElement);
var
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  i         : Integer;
  Field     : TField;
  TagName   : string;
  BM        : TBytes;
  MS        : TMemoryStream;
  FS        : TFormatSettings;
  Counter   : Integer;
begin
  FS := Sys.InvariantFormatSettings;

  TopNode['TableName'] := Xml.StrToXml(GetStrProp(Table, 'TableName'));

  Node := Xml.AddNode(TopNode, 'data');

  Table.Last;
  Table.First;
  Node['row_count'] := Table.RecordCount.ToString();

  Counter := 0;
  Table.DisableControls;
  BM := Table.Bookmark;
  try
    Table.First;
    while not Table.Eof do
    begin
      SubNode := Xml.AddNode(Node, 'row');

      for i := 0 to Table.FieldCount - 1 do
      begin
        Field   := Table.Fields[i];
        if Field.FieldKind = fkData then
        begin
          TagName := 'c' + IntToStr(i);
          if Field.IsNull then
            SubNode[TagName] := NULL_STR
          else begin
            case Field.DataType of
              ftString     ,
              ftWideString ,
              ftFixedWideChar,
              ftFixedChar  ,
              ftGuid       : SubNode[TagName] := StrToXml(Field.AsString);

              ftSmallint   ,
              ftInteger    ,
              ftWord       ,
              ftLargeint   ,
              ftAutoInc    : SubNode[TagName] := Field.AsString;

              ftBoolean    : SubNode[TagName] := AnsiLowerCase(Field.AsString);

              ftFloat      ,
              ftBCD        ,
              ftFMTBcd     ,
              ftCurrency   : SubNode[TagName] := FloatToStr(Field.AsFloat, FS); //  CommaToDot(Field.AsString);
              ftDate       : SubNode[TagName] := FormatDateTime('yyyy-mm-dd', Field.AsDateTime);
              ftTime       : SubNode[TagName] := FormatDateTime('hh:nn:ss', Field.AsDateTime);
              ftDateTime   ,
              ftTimeStamp  : SubNode[TagName] := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Field.AsDateTime);  //   hh:nn:ss:zzz
              ftMemo       ,
              ftFmtMemo    ,
              ftWideMemo   ,
              ftGraphic    ,
              ftOraBlob    ,
              ftOraClob    ,
              ftBlob       : begin
                               MS := TMemoryStream.Create;
                               try
                                 TBlobField(Field).SaveToStream(MS);
                                 MS.Position := 0;
                                 SubNode[TagName] := Sys.StreamToBase64(MS);
                               finally
                                 MS.Free;
                               end;

                             end;
            end;
          end;
        end;

      end;


      Table.Next;
      Counter := Counter + 1;
      if (Counter mod 100 = 0) then
        Sys.ProcessMessages();
    end;
  finally
    Table.Bookmark := BM;
    Table.EnableControls;
  end;

end;

class procedure Xml.XmlToDatasetData(Table: TDataset; TopNode: TDOMElement);
var
  Node      : TDOMElement;
  SubNode   : TDOMElement;
  Nodes     : TDOMNodeList;
  i         : Integer;
  j         : Integer;
  Field     : TField;
  MS        : TStream;
  sValue    : string;
  FS        : TFormatSettings;
  Counter   : Integer;

begin
  FS := Sys.InvariantFormatSettings;

  if Reflection.HasProperty(Table, 'TableName')  and (GetStrProp(Table, 'TableName') = '') and (Xml.GetAttr(TopNode, 'TableName', '') <> '') then
    SetStrProp(Table, 'TableName', (Xml.GetAttr(TopNode, 'TableName', '')));

  Node  := TopNode.FindNode('data') as TDOMElement;
  Nodes := Node.ChildNodes;

  Counter := 0;
  for j := 0 to Nodes.Count - 1 do
  begin
    SubNode := Nodes[j] as TDOMElement;
    Table.Append();

    for i := 0 to Table.FieldCount - 1 do
    begin
      sValue := Xml.GetAttr(SubNode, 'c' + IntToStr(i), NULL_STR);

      Field  := Table.Fields[i];
      if sValue = NULL_STR then
        Field.Value := Variants.Null
      else begin
        case Field.DataType of
          ftString     ,
          ftWideString ,
          ftFixedWideChar,
          ftFixedChar  ,
          ftGuid       : Field.Value := sValue;

          ftSmallint   ,
          ftInteger    ,
          ftWord       ,
          ftLargeint   ,
          ftAutoInc    : Field.Value  := sValue;

          ftBoolean    : Field.Value  := sValue; // = 'true';
          ftFloat      ,
          ftBCD        ,
          ftFMTBcd     ,
          ftCurrency   : Field.Value     := StrToFloat(sValue, FS);

          ftDate       : Field.Value     := StrToDate(sValue, FS);
          ftTime       : Field.Value     := StrToTime(sValue, FS);
          ftDateTime   ,
          ftTimeStamp  : Field.Value     := StrToDateTime(sValue, FS);
          ftMemo       ,
          ftWideMemo   ,
          ftFmtMemo    ,
          ftGraphic    ,
          ftOraBlob    ,
          ftOraClob    ,
          ftBlob       : begin
                           MS := Sys.Base64ToStream(sValue);
                           try
                             MS.Position := 0;
                             TBlobField(Field).LoadFromStream(MS);
                           finally
                             MS.Free;
                           end;

                         end;
        end;
      end;

    end;

    try
      Table.Post;
    except
      on E: Exception do
      begin
        E.Message := Format('XmlToDatasetData: Error while posting. Record number %d', [j]) + LB + E.Message;
        raise;
      end;
    end;

    Counter := Counter + 1;
    if (Counter mod 100 = 0) then
        Sys.ProcessMessages();
  end;

end;

class procedure Xml.DatasetToXml(Table: TDataset; TopNode: TDOMElement);
begin
  DatasetSchemaToXml(Table, TopNode);
  DatasetDataToXml(Table, TopNode);
end;

class function Xml.DatasetToXml(Table: TDataset; Encoding : string = EncodingUtf8): TXMLDocument;
begin
   Result := Xml.CreateDoc('datapacket', Encoding);
   DatasetToXml(Table, Result.DocumentElement);
end;

class function Xml.DatasetToXmlText(Table: TDataset; Encoding: string): string;
var
  Doc: TXMLDocument;
  SS: TStringStream;
begin
  Doc := Xml.DatasetToXml(Table, Encoding);
  try
    SS := TStringStream.Create('');
    try
      WriteXMLFile(Doc, SS);
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  finally
    Doc.Free;
  end;


end;

class procedure Xml.XmlTextToDataset(Table: TDataset; XmlText: string; CreateDataset: TProcedureMethod);
var
  Doc: TXMLDocument;
  SS: TStringStream;
begin
  SS := TStringStream.Create(XmlText);
  try
    ReadXmlFile(Doc, SS);
    try
      XmlToDataset(Table, Doc, CreateDataset);
    finally
      Doc.Free;
    end;
  finally
    SS.Free;
  end;
end;

class procedure Xml.XmlToDataset(Table: TDataset; TopNode: TDOMElement; CreateDataset: TProcedureMethod);
begin
  if not (Assigned(Table) and Assigned(TopNode) and Assigned(CreateDataset)) then
    raise Exception.Create('XmlToDataset: invalid params');

  XmlToDatasetSchema(Table, TopNode);

  CreateDataset();

  Table.Active := True;

  XmlToDatasetData(Table, TopNode);
end;

class procedure Xml.XmlToDataset(Table: TDataset; Doc: TXMLDocument; CreateDataset: TProcedureMethod);
begin
  if not (Assigned(Table) and Assigned(Doc) and Assigned(CreateDataset)) then
    raise Exception.Create('XmlToDataset: invalid params');

  XmlToDataset(Table, Doc.DocumentElement, CreateDataset);
end;

class procedure Xml.DatasetToXmlFile(Table: TDataset; const FileName: string; Encoding: string);
var
  Doc   : TXMLDocument;
begin
  Doc := Xml.CreateDoc('datapacket', Encoding);
  try
    DatasetToXml(Table, Doc.DocumentElement);
    WriteXMLFile(Doc, FileName);
  finally
    Doc.Free();
  end;

end;

class procedure Xml.DatasetFromXmlFile(Table: TDataset; const FileName: string; CreateDataset: TProcedureMethod);
var
  Doc   : TXMLDocument;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);

  ReadXMLFile(Doc, FileName);
  try
    XmlToDataset(Table, Doc, CreateDataset);
  finally
    Doc.Free();
  end;
end;

class procedure Xml.DatasetToXmlStream(Table: TDataset; Stream: TStream; Encoding: string);
var
  Doc   : TXMLDocument;
begin
  Doc := Xml.DatasetToXml(Table, Encoding);
  try
    WriteXMLFile(Doc, Stream);
  finally
    Doc.Free();
  end;

end;

class procedure Xml.DatasetFromXmlStream(Table: TDataset; Stream: TStream; CreateDataset: TProcedureMethod);
var
  Doc   : TXMLDocument;
begin
  ReadXMLFile(Doc, Stream);
  try
    Xml.XmlToDataset(Table, Doc, CreateDataset);
  finally
    Doc.Free();
  end;

end;


(*----------------------------------------------------------------------------
 Returns the type of graphic the stream may contains
 ----------------------------------------------------------------------------*)
class function Depictor.GetGraphicType(Stream: TStream): TGraphicType;
var
  Buf : Word;
begin
  Result := gtNone;          // DBCtrls
  if Assigned(Stream) and (Stream.Size > SizeOf(Buf)) then
  begin
    Stream.Position := 0;
    Stream.Read(Buf, SizeOf(Buf));
    case Buf of
      $0000 : Result  := gtICO;
      $0001 : Result  := gtBMP;
      $4D42 : Result  := gtBMP;
      //$CDD7 : Result  := gtWMF;
      $D8FF : Result  := gtJPG;
    end;
  end;
end;
(*----------------------------------------------------------------------------
 Tries to recognize the image format, the  Stream data contains.
 Loads Stream contents into Graphic by creating the appropriate Graphic object.
 ----------------------------------------------------------------------------*)
 class function Depictor.LoadGraphicFromStream(Stream: TStream; var Graphic: TGraphic): TGraphicType;
var
  GraphicType: TGraphicType;
begin
  Result := gtNone;

  GraphicType := GetGraphicType(Stream);

  case GraphicType of
    gtICO : Graphic := TIcon.Create;
    gtBMP : Graphic := TBitmap.Create;
    //gtWMF : Graphic := TMetafile.Create;
    gtJPG : Graphic := TJPEGImage.Create;
    else    Graphic := nil;
  end;

  if Assigned(Graphic) then
  try
    Stream.Position := 0;
    Graphic.LoadFromStream(Stream);
    Result := GraphicType;
  except
    Graphic.Free;
    Graphic := nil;
  end;

end;
(*----------------------------------------------------------------------------*)
class function Depictor.LoadPictureFromStream(Stream: TStream; Picture: TPicture): TGraphicType;
var
  Graphic   : TGraphic;
begin
  Result   := gtNone;
  Graphic  := nil;

  if Assigned(Stream) and Assigned(Picture) then
  begin
    Result := LoadGraphicFromStream(Stream, Graphic);
    Picture.Assign(Graphic);  // Graphic can be NIL, no problem with that
    FreeAndNil(Graphic);
  end;

end;
(*----------------------------------------------------------------------------*)
class function Depictor.LoadPictureFromField(Field: TBlobField; Picture: TPicture): TGraphicType;
var
  MS : TMemoryStream;
begin
  Result   := gtNone;

  if Assigned(Field) and (not Field.IsNull) then
  begin
    MS := TMemoryStream.Create;
    try
      Field.SaveToStream(MS);
      MS.Position := 0;
      Result := LoadPictureFromStream(MS, Picture);
    finally
      MS.Free;
    end;
  end;
end;




{ HexConverter }
(*----------------------------------------------------------------------------*)
class function HexConverter.InsertLineBreaks(Text: string; LineLength: Integer; Quoted: Boolean): string;
var
  Zeros      : string;
  S          : string;
  IsLastLine : Boolean;
  i          : Integer;
  TextLen    : Integer;
begin


  Result     := '';
  Zeros      := StringOfChar('0', LineLength);
  S          := '';
  TextLen    := Length(Text);

  i := 1;

  while True do
  begin
    IsLastLine := not ((i + LineLength) < TextLen);
    S := Copy(Text, i, LineLength);

    if not (Zeros = S) then
    begin
      case Quoted of
        False : case IsLastLine of
                  False : Result := Result  + S  + #13;   //    #10
                  True  : Result := Result  + S + ';';
                end;
        True  : case IsLastLine of
                  False : Result := Result + '''' + S + '''' + ' + ' + #13;   //    #10
                  True  : Result := Result + '''' + S + ''';';
                end;
      end;
    end;

    i := i + LineLength;
    if i > TextLen then
      Break;

    Sys.ProcessMessages;
  end;

end;



(*----------------------------------------------------------------------------*)
class function HexConverter.NormalizeHexText(Text: string): string;
const
  cHexDigits : set of Char = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];
var
  i    : Integer;
  P    : PChar;
  S    : string;
  Size : Integer;
  Len  : Integer;
begin
  Result := Text;
  Len    := Length(Text);

  if Len > 0 then
  begin
    SetString(S, nil, Length(Text));

    Size := 0;
    P := PChar(S);

    for i := 1 to Len do
      if (Text[i] in cHexDigits) then
      begin
        P^ := Text[i];
        Inc(P);
        Inc(Size);
      end;

    SetLength(S, Size);

    Result := S;
  end;

end;
(*----------------------------------------------------------------------------*)
class function HexConverter.StripLineBreaks(Text: string; LineBreak: string): string;
var
  Start : Integer;
  L     : Integer;
  P     : Integer;
begin

  Start := 1;
  L := Length(LineBreak);
  P := Pos(LineBreak, Text);
  Result := '';

  while (P > 0) do
  begin
    Result := Result + Copy(Text, Start, P - Start);
    Start := P + L;
    P := Sys.PosEx(LineBreak, Text, Start);
    Sys.ProcessMessages;
  end;

  if (Start < Length(Text)) then
    Result := Result + Copy(Text, Start, Length(Text) - Start);

end;
(*----------------------------------------------------------------------------*)
class function HexConverter.StripLineBreaks(Text: string): string;
begin
  Result := StripLineBreaks(Text, #13#10);
end;

class function HexConverter.BinToHex(Buffer: Pointer; Size: UInt32): string;
begin
  SetLength(Result, Size * 2);

  Classes.BinToHex(PChar(Buffer), PChar(Result), Size);
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.BinToHex(Stream: TStream): string;
var
  MS : TMemoryStream;
begin

  SetLength(Result, (Stream.Size - Stream.Position) * 2);

  if Length(Result) > 0 then
  begin
    if (Stream is TMemoryStream) then
      MS := TMemoryStream(Stream)
    else
      MS := TMemoryStream.Create;

    try
      if MS <> Stream then
      begin
        MS.CopyFrom(Stream, Stream.Size - Stream.Position);
        MS.Position := 0;
      end;
      //Classes.BinToHex(PChar(UInt32(MS.Memory) + MS.Position), PChar(Result), MS.Size - MS.Position);
      Result := BinToHex(MS.Memory,  MS.Size - MS.Position);
    finally
      if Stream <> MS then
        MS.Free;
    end;
  end;

end;
(*----------------------------------------------------------------------------*)
class function  HexConverter.BinToHex(Field: TBlobField): string;
var
  MS: TMemoryStream;
begin
  if Field.IsNull then
    Result := ''
  else begin
    MS := TMemoryStream.Create;
    try
      Field.SaveToStream(MS);
      MS.Position := 0;
      Result := BinToHex(MS);
    finally
      MS.Free;
    end;

  end;
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.BinToHex(Stream: TStream; LineLength: Integer; Quoted: Boolean): string;
begin
  Result := InsertLineBreaks(BinToHex(Stream), LineLength, Quoted);
end;
(*----------------------------------------------------------------------------*)
class procedure HexConverter.BinToHex(Stream: TStream; List: TStrings; LineLength: Integer; Quoted: Boolean);
begin
  List.Text := BinToHex(Stream, LineLength, Quoted);
end;
(*----------------------------------------------------------------------------*)
class procedure HexConverter.BinToHex(Bitmap: TBitmap; List: TStrings; LineLength: Integer; Quoted: Boolean);
var
  Graphic: TGraphic;
begin
  Graphic := Bitmap;
  BinToHex(Graphic, List, LineLength, Quoted);
end;
(*----------------------------------------------------------------------------*)
class procedure HexConverter.BinToHex(Graphic: TGraphic; List: TStrings; LineLength: Integer; Quoted: Boolean);
var
  MS : TMemoryStream;
begin
  if Graphic <> nil then
  begin
    MS := TMemoryStream.Create;
    try
      Graphic.SaveToStream(MS);
      MS.Position := 0;
      BinToHex(MS, List, LineLength, Quoted);
    finally
      MS.Free;
    end;

  end;
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.HexToBin(Text: string; Stream: TStream): Integer;
var
  MS  : TMemoryStream;
  Pos : Integer;
begin
  Text := NormalizeHexText(Text);

  if Text <> '' then
  begin
    if Stream is TMemoryStream then
      MS := TMemoryStream(Stream)
    else
      MS := TMemoryStream.Create;

    try
      Pos := MS.Position;
      MS.SetSize(MS.Size + Length(Text) div 2);
      Classes.HexToBin(PChar(Text), PChar(Integer(MS.Memory) + MS.Position), Length(Text) div 2);
      MS.Position := Pos;
      if Stream <> MS then
        Stream.CopyFrom(MS, Length(Text) div 2);
      Result := MS.Size - Pos;
    finally
      if Stream <> MS then
        MS.Free;
    end;
  end
  else
    Result := 0;

end;
(*----------------------------------------------------------------------------*)
class function  HexConverter.HexToBin(Text: string; Field: TBlobField): Integer;
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    Field.Clear;
    HexToBin(Text, MS);
    MS.Position := 0;
    if MS.Size > 0 then
      Field.LoadFromStream(MS);

    Result := MS.Size;
  finally
    MS.Free;
  end;

end;
(*----------------------------------------------------------------------------*)
class function HexConverter.HexToBin(Text: string; var Graphic: TGraphic): Boolean;
var
  MS : TMemoryStream;
begin
  Graphic := nil;

  MS := TMemoryStream.Create;
  try
    HexToBin(Text, MS);
    MS.Position := 0;
    Result := LoadGraphicFromStream(MS, Graphic);
  finally
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.HexToBin(Text: string; var Bitmap: TBitmap): Boolean;
var
  Graphic: TGraphic;
begin
  Bitmap  := nil;
  Result  := HexToBin(Text, Graphic);

  if Result then
  begin
    if (Graphic is TBitmap) then
      Bitmap := TBitmap(Graphic)
    else begin
      Bitmap := TBitmap.Create;
      Bitmap.Assign(Graphic);
      Graphic.Free;
    end;
  end;
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.HexToBin(Text: string; Picture: TPicture): Boolean;
var
  Graphic: TGraphic;
begin
  Result := HexToBin(Text, Graphic);
  Picture.Assign(Graphic);
  if (Result and (Graphic <> nil))  then
    Graphic.Free;
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.StrToHex(const Text: string): string;
var
  Size : Integer;
begin
  Size := Length(Text);
  SetLength(Result, Size * 2);
  Classes.BinToHex(PChar(Text), PChar(Result), Size);
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.HexToStr(const Text: string): string;
var
  Size : Integer;
begin
  Size := Length(Text) div 2;
  SetLength(Result, Size);
  Classes.HexToBin(PChar(Text), PChar(Result), Size);
end;
(*----------------------------------------------------------------------------*)
class function HexConverter.LoadGraphicFromStream(Stream: TStream; var Graphic: TGraphic): Boolean;
begin
  Result := Depictor.LoadGraphicFromStream(Stream, Graphic) <> gtNone;
end;










type
(*----------------------------------------------------------------------------*)
  TObjectBox = class(TInterfacedObject, IObjectBox)
  private
    FInstance : TObject;
    FOwnsInstance : Boolean;

    function get_Instance: TObject;
    function get_OwnsInstance: Boolean;
  public
    constructor Create(Instance: TObject; OwnsIstance: Boolean);
    destructor Destroy; override;

    property Instance     : TObject read get_Instance;
    property OwnsIstance  : Boolean read get_OwnsInstance;
  end;


{ TObjectBox }
(*----------------------------------------------------------------------------*)
constructor TObjectBox.Create(Instance: TObject; OwnsIstance: Boolean);
begin
  inherited Create;
  FInstance := Instance;
  FOwnsInstance := OwnsIstance;
end;
(*----------------------------------------------------------------------------*)
destructor TObjectBox.Destroy;
begin
  if FOwnsInstance then
    try
      FreeAndNil(FInstance)
    except
    end;

  inherited;
end;
(*----------------------------------------------------------------------------*)
function TObjectBox.get_Instance: TObject;
begin
  Result := FInstance;
end;
(*----------------------------------------------------------------------------*)
function TObjectBox.get_OwnsInstance: Boolean;
begin
  Result := FOwnsInstance;
end;




{ Sys }
(*----------------------------------------------------------------------------*)
class constructor Sys.Create;
const
  FormatSettings : TFormatSettings = (
      CurrencyFormat: 1;
      NegCurrFormat: 5;
      ThousandSeparator: ',';
      DecimalSeparator: '.';
      CurrencyDecimals: 2;
      DateSeparator: '-';
      TimeSeparator: ':';
      ListSeparator: ',';
      CurrencyString: '$';
      ShortDateFormat: 'yyyy-mm-dd';          // 'd/m/y';
      LongDateFormat: 'yyyy-mm-dd';           // 'dd" "mmmm" "yyyy';
      TimeAMString: 'AM';
      TimePMString: 'PM';
      ShortTimeFormat: 'hh:nn';
      LongTimeFormat: 'hh:nn:ss';
      ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                        'Jul','Aug','Sep','Oct','Nov','Dec');
      LongMonthNames: ('January','February','March','April','May','June',
                       'July','August','September','October','November','December');
      ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
      LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
      TwoDigitYearCenturyWindow: 50;
    );
begin
  FAppPath := Paramstr(0);
  FAppFolder := ExtractFilePath(FAppPath);
  FAppExeName   := ChangeFileExt(ExtractFileName(FAppPath), '');

  FInvariantFormatSettings := FormatSettings;




end;
(*----------------------------------------------------------------------------*)
class destructor Sys.Destroy;
begin

end;

(*----------------------------------------------------------------------------*)
class function Sys.GetAppDataFolder: string;
begin
  Result := NormalizePath(CombinePath(AppFolder, 'Data'))
end;


(*----------------------------------------------------------------------------*)
class procedure Sys.Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;
(*----------------------------------------------------------------------------*)
class procedure Sys.Error(const Msg: string; const Args: array of const);
begin
   Error(Format(Msg, Args));
end;
(*----------------------------------------------------------------------------*)
class procedure  Sys.ErrorNotYet(const Msg: string);
begin
  Error(Msg + ' - Not yet implemented');
end;


(*----------------------------------------------------------------------------*)
class function Sys.StringToBase64(Input: string): string;
begin
  Result := EncodeStringBase64(Input);
end;





(*----------------------------------------------------------------------------*)
class function Sys.StreamToBase64(Input: TStream): string;
var
  Output: TStringStream;
  Encoder : TBase64EncodingStream;
begin
  Result := '';

  if (Input.Size > 0) then
  begin
    Input.Position := 0;

    Output := TStringStream.Create();
    try
      Encoder := TBase64EncodingStream.Create(Output);
      try
        Encoder.CopyFrom(Input, Input.Size);
        Result := Output.DataString;
      finally
        Encoder.Free;
      end;
    finally
       Output.Free;
    end;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.Base64ToString(Input: string): string;
begin
  Result := DecodeStringBase64(Input);
end;

function DecodeStringBase64(const s:string):String;

var instream,outstream : TStringStream;
    decoder : TBase64DecodingStream;
begin
  instream:=TStringStream.Create(s);
  try
    outstream:=TStringStream.Create('');
    try
      decoder:=TBase64DecodingStream.create(instream,bdmmime);
      try
         outstream.copyfrom(decoder,decoder.size);
         outstream.position:=0;
         result:=outstream.readstring(outstream.size);
      finally
        decoder.free;
        end;
    finally
     outstream.free;
     end;
  finally
    instream.free;
    end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.Base64ToStream(Input: string): TStream;
var
  InputSS : TStringStream;
  Decoder : TBase64DecodingStream;
begin
  Result  := TMemoryStream.Create;
  InputSS := TStringStream.Create(Input);
  try
    InputSS.Position := 0;
    Decoder := TBase64DecodingStream.Create(InputSS, bdmMIME);
    Result.CopyFrom(Decoder, Decoder.Size);
    Result.Position := 0;
  finally
    InputSS.Free;
  end;
end;

class function Sys.IsSameText(A: string; B: string): Boolean;
begin
  Result := AnsiSameText(A, B);
end;

class function Sys.IsEmpty(const S: string): Boolean;
begin
  Result := IsEmptyStr(S, [#0..#32]);
end;

(*----------------------------------------------------------------------------*)

class function Sys.PathCombine(A: string; B: string): string;
begin
  Result := ConcatPaths([A, B]);
end;

class function Sys.IsLetter(C: Char): Boolean;
begin
  Result := C in SetOfChars;
end;

(*----------------------------------------------------------------------------*)
class function Sys.LCopy(const Text: string; const Index: Integer): string;
begin
  Result := Text;
  if Length(Result) > 0 then
    if Index <= Length(Result) then
      Result := Copy(Result, 1, Index - 1);
end;
(*----------------------------------------------------------------------------*)
class function Sys.RCopy(const Text: string; const Index: Integer): string;
begin
  Result := Text;
  if Length(Result) > 0 then
    if Index <= Length(Result) then
      Result := Copy(Result, Index + 1, Length(Result) - Index);
end;
(*--------------------------------------------------------------------------------*)
class function Sys.LPad(const S: string; C: WideChar; Len: Integer): string;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := StringOfChar(C, Len - L) + S
  else Result := S;
end;
(*----------------------------------------------------------------------------*)
class function Sys.RPad(const S: string; C: WideChar; Len: Integer): string;
var
  L: Integer;
begin
  L := Length(S);
  if L < Len then
    Result := S + StringOfChar(C, Len - L)
  else Result := S;
end;
(*----------------------------------------------------------------------------*)
class function Sys.Split(S: string; Delim: Char): TStringList;
begin
  Result := TStringList.Create();
  Split(S, Delim, Result);
end;
(*----------------------------------------------------------------------------*)
class procedure  Sys.Split(S: string; Delim: Char; List: TStringList);
var
  i    : integer;
  Temp : string;
begin
  Temp := '';

  for i := 1 to Length(S) do
    if S[i] = Delim then
    begin
      if Length(Temp) > 0 then
        List.Add(Temp);
      Temp := '';
    end
    else Temp := Temp + S[i];

  if Length(Temp) > 0 then
    List.Add(Temp);
end;
(*----------------------------------------------------------------------------
 Splits S into LeftArg and RightArg based on the first occurence of Delim
 ----------------------------------------------------------------------------*)
class procedure Sys.Split(S: string; Delim: string; var LeftArg: string; var RightArg: string);
var
  FoundPos      : Integer;
  SeparatorLen  : Integer;
begin

  SeparatorLen  := Length(Delim);
  FoundPos  := Pos(Delim, S);
  if FoundPos > 0 then
  begin
    LeftArg   := LeftStr(S, FoundPos - 1);
    RightArg  := RCopy(S, FoundPos + SeparatorLen);
    //RightArg  := MidToEndStr(SourceStr, FoundPos + SeparatorLen);
  end
  else
  begin
    LeftArg   := S;
    RightArg  := '';
  end;
end;

(*----------------------------------------------------------------------------*)
class function Sys.QS(const S: string): string;
begin
  Result := AnsiQuotedStr(S, '''');
end;
(*----------------------------------------------------------------------------*)
class function Sys.AppendCRLF(const S: string): string;
begin
  Result := S;

  if Length(Result) > 0 then
  begin
    if not AnsiEndsStr(CRLF, S)  then
      Result := Result + CRLF;
  end else
    Result := CRLF;
end;
(*----------------------------------------------------------------------------*)
class function Sys.StripCRLFs(const S: string): string;
begin
  Result := S;

  if Length(Result) > 0 then
    Result := AnsiReplaceText(Result, CRLF, ' ');
end;
(*----------------------------------------------------------------------------*)
class function Sys.UnicodeToAnsi(const S: string): AnsiString;
var
  Bytes : TBytes;
begin
  Result := '';
  if (Length(S) > 0) and (TEncoding.Default <> TEncoding.Unicode) then
  begin
    Bytes := TEncoding.Unicode.GetBytes(S);
    Bytes := TEncoding.Convert(TEncoding.Unicode, TEncoding.Default, Bytes);
    SetLength(Result, Length(Bytes));
    Move(Pointer(Bytes)^, Pointer(Result)^, Length(Bytes));
  end;
end;
(*----------------------------------------------------------------------------*)
class function  Sys.LoadFromFile(const FileName: string): string;
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    if FileExists(FileName) then
      List.LoadFromFile(FileName);
    Result := List.Text;
  finally
    List.Free;
  end;

end;
(*----------------------------------------------------------------------------*)
class procedure Sys.SaveToFile(const FileName: string; const Data: string);
var
  List : TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := Data;
    List.SaveToFile(FileName);
  finally
    List.Free;
  end;

end;
{-----------------------------------------------------------------------------------
 writes a string to an existent or non existent file
-----------------------------------------------------------------------------------}
class procedure Sys.WriteToFile(const FileName, S: string);
var
  F : TextFile;
begin
  AssignFile(F,FileName);
  try
    if FileExists(FileName)  then
      Append(F) else
    ReWrite(F);
    WriteLn(F,S);
  finally
    CloseFile(F);
  end;
end;

class function Sys.LoadTextFromStream(Stream: TStream): string;
begin
  Result := '';
  Stream.Seek(0, soBeginning);
  SetLength(Result, Stream.Size);
  Stream.Read(Pointer(Result)^, Stream.Size);
end;

class procedure Sys.SaveTextToStream(Stream: TStream; Data: string);
begin
   Stream.WriteBuffer(Pointer(Data)^, Length(Data));
end;

(*----------------------------------------------------------------------------*)
class function  Sys.IsValidFileName(const FileName: string): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Trim(FileName) = '' then Exit; //==>
  for i := 1 to Length(FileName) do
    if CharInSet(FileName[i], ['\', '/', ':','*',  '?', '''', '"', '<', '>', '|']) then
      Exit; //==>

  Result := True;
end;
(*----------------------------------------------------------------------------*)
class function  Sys.StrToValidFileName(const S: string): string;
var
  i : Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if CharInSet(Result[i], ['\', '/', ':','*',  '?', '''','"', '<', '>', '|']) then
      Result[i] := ' ';
end;

class function Sys.CreateGuid(UseBrackets: Boolean): string;
var
  Guid: TGuid;
begin
   if SysUtils.CreateGUID(Guid) <> 0 then
      raise Exception.Create('Failed to create GUID');

   Result := GUIDToString(Guid);

   if not UseBrackets then
   begin
     SetLength(Result, Length(Result)-1);
     Delete(Result, 1, 1);
   end;
end;

(*----------------------------------------------------------------------------
{ PosEx searches for SubStr in S and returns the index position of
  SubStr if found and 0 otherwise.  If Offset is not given then the result is
  the same as calling Pos.  If Offset is specified and > 1 then the search
  starts at position Offset within S.  If Offset is larger than Length(S)
  then PosEx returns 0.  By default, Offset equals 1.  }
 ----------------------------------------------------------------------------*)
class function Sys.PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

(*----------------------------------------------------------------------------*)
class function Sys.DoubleToStr(Value: Extended; Digits: integer): string;
begin
  Result := FloatToStrF(Value, ffFixed, 15, Digits);
end;
(*----------------------------------------------------------------------------*)
class function Sys.DoubleToStrSQL(Value: Extended; Digits: integer): string;
var
  DecSep : Char;
begin
  // TODO: DoubleToStrSQL
  DecSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := FloatToStrF(Value, ffFixed, 15, Digits);
  finally
    DecimalSeparator := DecSep;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.FormatMoney(Value: Extended;  bCurrencyString: Boolean): string;
var
  i : integer;
begin
  // TODO: FormatMoney
  Result := Format('%m', [Value]);

  if Frac(Value) = 0 then begin
    i := Pos(DecimalSeparator, Result);
    SetLength(Result, i - 1);
    if bCurrencyString then
      Result := Result + ' ' + CurrencyString;
  end else begin
    if not bCurrencyString then
    begin
      i := Pos(' ', Result);
      SetLength(Result, i - 1);
    end;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.NormalDecSep(const S: string): string;
var
  iIndex : integer;
  C      : char;
begin
  // TODO: NormalDecSep
  Result := S;
  if DecimalSeparator = '.' then C := ',' else C := '.';

  iIndex := Pos(C, S);
  if iIndex <> 0 then Result[iIndex] := DecimalSeparator;
  if iIndex = 1 then Result := '0' + Result;
end;
(*----------------------------------------------------------------------------*)
class function Sys.DotToComma(const sFloat: string): string;
var
  iIndex : integer;
begin
  Result := sFloat;
  iIndex := Pos('.', Result);
  if iIndex > 0 then Result[iIndex] := ',';
end;
(*----------------------------------------------------------------------------*)
class function Sys.CommaToDot(const sFloat: string): string;
var
  iIndex : integer;
begin
  Result := sFloat;
  iIndex := Pos(',', Result);
  if iIndex > 0 then Result[iIndex] := '.';
end;
(*----------------------------------------------------------------------------
 Tries to get the result first in local and then in global format
 by using the InvariantFormatSettings
 ----------------------------------------------------------------------------*)
class function  Sys.StrToDateSafe(const S: string; Default: TDate): TDate;
var
  DT: TDateTime;
begin
  DT := Default;
  if not TryStrToDate(S, DT) then
    TryStrToDate(S, DT, InvariantFormatSettings);
  Result := DT;
end;
(*----------------------------------------------------------------------------
 Tries to get the result first in local and then in global format
 by using the InvariantFormatSettings
 ----------------------------------------------------------------------------*)
class function  Sys.StrToTimeSafe(const S: string; Default: TTime): TTime;
var
  DT: TDateTime;
begin
  DT := Default;
  if not TryStrToTime(S, DT) then
    TryStrToTime(S, DT, InvariantFormatSettings);
  Result := DT;
end;
(*----------------------------------------------------------------------------
 Tries to get the result first in local and then in global format
 by using the InvariantFormatSettings
 ----------------------------------------------------------------------------*)
class function  Sys.StrToDateTimeSafe(const S: string; Default: TDateTime): TDateTime;
var
  DT: TDateTime;
begin
  DT := Default;
  if not TryStrToDateTime(S, DT) then
    TryStrToDateTime(S, DT, InvariantFormatSettings);
  Result := DT;
end;
(*----------------------------------------------------------------------------*)
class function Sys.DateToStrSQL(ADate: TDateTime; bQuoted: boolean): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', ADate);
  if bQuoted then Result := Format('''%s''', [Result]);
end;
(*----------------------------------------------------------------------------*)
class function Sys.TimeToStrSQL(ATime: TDateTime; bQuoted: boolean): string;
begin
  Result := FormatDateTime('hh:nn', ATime);
  if bQuoted then Result := Format('''%s''', [Result]);
end;
(*----------------------------------------------------------------------------*)
class function Sys.DateTimeToStrSQL(ADateTime: TDateTime; bQuoted: boolean): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn', ADateTime);
  if bQuoted then  Result := Format('''%s''', [Result]);
end;
(*----------------------------------------------------------------------------*)
class function Sys.TimeStamp(ADateTime: TDateTime;  bQuoted: boolean): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', ADateTime);
  if bQuoted then  Result := Format('''%s''', [Result]);
end;
(*----------------------------------------------------------------------------*)
class function Sys.ExtractDate(DT: TDateTime): TDateTime;
var
  Hour, Min_, Sec, MSec: Word;
begin
  Result := DT;
  DecodeTime(Result, Hour, Min_, Sec, MSec);
  Result := EncodeTime(Hour, Min_, Sec, MSec);
end;
(*----------------------------------------------------------------------------*)
class function Sys.ExtractTime(DT: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  Result := DT;
  DecodeDate(Result, Year, Month, Day);
  Result := EncodeDate(Year, Month, Day);
end;
(*----------------------------------------------------------------------------*)
class function Sys.DateTimeToFileName(const DT: TDateTime; UseMSecs: Boolean): string;
begin
  case UseMSecs of
    False : Result := FormatDateTime('yyyy-mm-dd hh_nn_ss', DT);
    True  : Result := FormatDateTime('yyyy-mm-dd hh_nn_ss__zzz', DT);
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.DateTimeToFileName2(const DT: TDateTime; UseMSecs: Boolean): string;
begin
  case UseMSecs of
    False : Result := FormatDateTime('yyyymmddhhnnss', DT);
    True  : Result := FormatDateTime('yyyymmddhhnnsszzz', DT);
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarIsNull(const Value: Variant): Boolean;
begin
  Result := Variants.VarIsNull(Value) or Variants.VarIsClear(Value) or Variants.VarIsEmpty(Value)
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarIsValid(const Value: Variant): Boolean;
begin
  Result := not Sys.VarIsNull(Value)
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarIsDisp(const V: Variant): Boolean;
begin
   Result := (TVarData(V).VType = varDispatch) and (TVarData(V).VDispatch <> nil)
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarIsUnk(const V: Variant): Boolean;
begin
  Result := (TVarData(V).VType = varUnknown) and (TVarData(V).VUnknown <> nil)
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarIsObj(const V: Variant): Boolean;
begin
  Result := VarIsDisp(V) or VarIsUnk(V) // or ((TVarData(V).VType and varTypeMask) = varObj)
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarEquals(const V1, V2: Variant): Boolean;
begin
  try
    //Result := V1 = V2;
    Result := VarSameValue(V1, V2);
  except
    Result := False;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.VarCompare(const Item1, Item2: Variant): Integer;
var
  Res : TVariantRelationship;
  Null1 : Boolean;
  Null2 : Boolean;
begin

  Null1 := Sys.VarIsNull(Item1);
  Null2 := Sys.VarIsNull(Item2);

  if Null1 and Null2 then
    Result := 0
  else if Null1 then
    Result := -1
  else if Null2 then
    Result := 1
  else begin
    Res := VarCompareValue(Item1, Item2);
    if Res = vrEqual then
      Result := 0
    else if Res = vrLessThan then
      Result := -1
    else
      Result := 1;
  end;

end;
(*----------------------------------------------------------------------------*)
class function Sys.VarToArray(const Source: Variant): TArrayOfVariant;
var
  i     : Integer;
  Size  : Integer;
begin
  Result := [];
  if VarIsArray(Source) then
  begin
    Size := VarArrayHighBound(Source, 1) + 1;
    SetLength(Result, Size);
    for i := VarArrayLowBound(Source, 1) to VarArrayHighBound(Source, 1) do
      Result[i] := Source[i];
  end else begin
    SetLength(Result, 1);
    Result[0] := Source;
  end;
end;
(*----------------------------------------------------------------------------*)
class function Sys.BoxObject(Instance: TObject; OwnsIstance: Boolean): IObjectBox;
begin
  Result := TObjectBox.Create(Instance, OwnsIstance);
end;
(*----------------------------------------------------------------------------*)
class function Sys.Box(Instance: TObject; OwnsIstance: Boolean): Variant;
begin
  Result := BoxObject(Instance, OwnsIstance);
end;
(*----------------------------------------------------------------------------*)
class function Sys.UnBox(Value: Variant): TObject;
begin
  if IsBox(Value) then
    Result := (IInterface(Value) as IObjectBox).Instance
  else
    Result := nil;
end;
(*----------------------------------------------------------------------------*)
class function Sys.IsBox(const Value: Variant): Boolean;
var
  Intf: IObjectBox;
begin
  Result := (not VarIsNull(Value)) and VarIsType(Value, varUnknown) and Variants.VarSupports(Value, IObjectBox, Intf);
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a stream into a "variant array of byte".
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class function Sys.StreamToVarArray(const Stream: TStream): Variant;
var
  P: Pointer;
begin
  Result := VarArrayCreate([0, Stream.Size - 1], varByte);
  Stream.Position := 0;

  P := VarArrayLock(Result);
  try
    Stream.Read(P^, Stream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a "variant array of byte" into a stream.
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class procedure Sys.VarArrayToStream(const V: Variant; const Stream: TStream);
var
  P: Pointer;
  L: Integer;
begin
  Assert(VarType(V) = varByte or varArray);
  Assert(VarArrayDimCount(V) = 1);

  L := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
  Stream.Size := L;
  Stream.Position := 0;

  P := VarArrayLock(V);
  try
    Stream.Write(P^, Stream.Size);
  finally
    VarArrayUnlock(V);
  end;
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a "dynamic array of byte" into a "variant array of byte".
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class function Sys.ByteArrayToVariant(const A: TBytes): Variant;
var
  L: Integer;
  P: Pointer;
begin
  L := Length(A);
  Result := VarArrayCreate([0, L - 1], varByte);

  P := VarArrayLock(Result);
  try
    Move(Pointer(A)^, P^, L);
  finally
    VarArrayUnlock(Result);
  end;
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a "variant array of byte" into a "dynamic array of byte".
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class function Sys.VariantToByteArray(const V: Variant): TBytes;
var
  L: Integer;
  P: Pointer;
begin
  Result := [];

  Assert(VarType(V) = varByte or varArray);
  Assert(VarArrayDimCount(V) = 1);

  L := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;

  SetLength(Result, L);
  P := VarArrayLock(V);
  try
    Move(P^, Pointer(Result)^, L);
  finally
    VarArrayUnlock(V);
  end;
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a "dynamic array of byte" into a stream.
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class procedure Sys.ByteArrayToStream(const A: TBytes; const Stream: TStream);
begin
  Stream.Position := 0;
  Stream.Size := Length(A);
  Stream.Write(Pointer(A)^, Length(A));
end;
(*----------------------------------------------------------------------------
  This procedure copies the content of a stream into a "dynamic array of byte".
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class function Sys.StreamToByteArray(const Stream: TStream): TBytes;
begin
  Result := [];
  Stream.Position := 0;
  SetLength(Result, Stream.Size);
  Stream.Read(Pointer(Result)^, Stream.Size);
end;
(*----------------------------------------------------------------------------
  This function returns a copy of a "dynamic array of byte".
  Code by Gary Williams
 ----------------------------------------------------------------------------*)
class function Sys.CopyByteArray(const A: TBytes): TBytes;
begin
  Result := [];
  SetLength(Result, Length(A));
  Move(Pointer(A)^, Pointer(Result)^, Length(A));
end;
class function  Sys.CombinePath(const A, B: string): string;
begin
  Result := ConcatPaths([A, B]);
end;

{----------------------------------------------------------------------------------
 Description    : if necessary adds an ending '\' and a ':' before Folder
-----------------------------------------------------------------------------------}
class function Sys.NormalizePath(const Path: string): string;
begin
  Result := Path;
  case Length(Result) of
    0 : {AppError('Path can not be an empty string')};
    1 : if CharInSet(UpCase(Result[1]), ['A'..'Z']) then
          Result := Result + ':\';
    2 : if CharInSet(UpCase(Result[1]), ['A'..'Z']) then
          if Result[2] = ':' then
            Result := Result + '\';
    else
        if not (Result[Length(Result)] = '\') then
           Result := Result + '\';
  end;
end;
(*----------------------------------------------------------------------------*)
class function  Sys.DenormalizePath(const Path: string): string;
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[Length(Result)] = '\') then
    SetLength(Result, Length(Result) - 1);
end;
(*----------------------------------------------------------------------------*)
class function Sys.EnsureExtension(FileName: string; Extension: string): string;
begin
  if (Length(Extension) > 0) and (not AnsiStartsText('.', Extension))  then
    Extension := '.' + Extension;

  Result := ChangeFileExt(FileName, Extension);
end;
(*----------------------------------------------------------------------------*)
class function Sys.FolderDelete(Folder: string): Boolean;
begin
   Result := False;
   // TODO: FolderDelete

end;
(*----------------------------------------------------------------------------*)
class function Sys.FolderCopy(Source, Dest: string; Overwrite: Boolean): Boolean;
begin
  Result := False;
  // TODO: FolderCopy
end;
(*----------------------------------------------------------------------------*)
class function Sys.FolderMove(Source, Dest: string; Overwrite: Boolean): Boolean;
begin
  Result := FolderCopy(Source, Dest, Overwrite);
  if Result then
    Result := FolderDelete(Source);
end;
(*----------------------------------------------------------------------------*)
// Example call   : FindFiles('C:\Windows\System', '*.d??', ListBox1.Items);
class procedure Sys.FindFiles(StartFolder, FileMask: string; List: TStrings; AddPath: Boolean = False; Recursive: Boolean = False);
  {------------------------------------------------------}
  procedure FindFilesInFolder(Folder: string);
  var
    SR     : TSearchRec;
    iCode  : Integer;
  begin

    iCode := SysUtils.FindFirst(Folder + FileMask, faAnyFile, SR);

    try
      if iCode = 0 then
        repeat
          if (SR.Name[1] <> '.') and
             (SR.Name <> '..') and
             (SR.Attr and faDirectory <> faDirectory) then
          begin
            if AddPath then
              List.Add(Folder + SR.Name)
            else
             List.Add(SR.Name);
          end;
        until SysUtils.FindNext(SR) <> 0;
    finally
      SysUtils.FindClose(SR);
    end;

  end;
  {------------------------------------------------------}
var
  SR       : TSearchRec;
  iCode    : Integer;
begin
  // TODO: FindFiles
  StartFolder := NormalizePath(StartFolder);
  FindFilesInFolder(StartFolder);

  iCode := FindFirst(StartFolder + '*.*', faAnyFile, SR);

  while iCode = 0 do
  begin
    if ((SR.Name <> '.') and (SR.Name <> '..')) then
      if (SR.Attr and faDirectory) > 0   then
        FindFiles(NormalizePath(StartFolder + SR.Name), FileMask, List, AddPath);

    iCode := FindNext(SR);
  end;

  SysUtils.FindClose(SR);

end;

{----------------------------------------------------------------------------------
 Description    : returns True if Filename has the FileAttr attribute
-----------------------------------------------------------------------------------}
class function Sys.HasAttribute(const FileName: string; FileAttr: integer): boolean;
var
  i : integer;
begin
  i := SysUtils.FileGetAttr(FileName);
  Result := (i >= 0) and (i and FileAttr = FileAttr);
end;
{----------------------------------------------------------------------------------
 Description :  returns a DOS date-time stamp for the specified file
-----------------------------------------------------------------------------------}


class function Sys.GetFileDate(const FileName: string): TDateTime;
var
  Handle: Int64;
begin
  Handle := FileOpen(FileName, 0);
  try
    try
      Result := SysUtils.FileDateToDateTime(SysUtils.FileGetDate(Handle));
    except
      Result := 0;
    end;
  finally
    FileClose(Handle);
  end;
end;
(*--------------------------------------------------------------------------------*)
class function Sys.SetFileDate(const FileName: string; NewDate: TDateTime): boolean;
var
  Handle: Int64;
begin
  Result := False;

  if NewDate < SysUtils.StrToDateTime('01/01/1980') then Exit;

  Handle := FileOpen(FileName, fmOpenWrite or fmShareDenyWrite);
  try
    Result := FileSetDate(Handle, DateTimeToFileDate(NewDate)) = 0;
  finally
    FileClose(Handle);
  end;
end;




(*----------------------------------------------------------------------------------
 Description    : returns the size of the FileName without opening the file.  If the file
                  doesn't exist, returns -1.
 Parameters     :
 Error checking : ?
 Notes          :  Int64 is defined as
                   type
                    {$IFDEF BT_D4_UP}
                       Int64 = Int64;
                    {$ELSE}
                      Int64 = Integer;
                    {$ENDIF}
 Author          : Theo Bebekis <bebekis@otenet.gr>
-----------------------------------------------------------------------------------*)
class function Sys.GetFileSize(const FileName: string): Int64;
var
  SR    : TSearchRec;
  iCode : integer;
begin
  Result := -1;
  iCode  := SysUtils.FindFirst(ExpandFileName(FileName), faAnyFile, SR);
  try
    if iCode = 0 then
    Result := Int64(SR.Size)
  finally
    SysUtils.FindClose(SR);
  end;
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.GetFolderSize(const Path: string): Int64;
var
  SR        : TSearchRec;
  sPath     : string;
  iCode     : integer;
begin
  // TODO: GetFolderSize
  Result := 0;

  sPath := NormalizePath(Path);
  iCode := SysUtils.FindFirst(sPath + '*.*', faAnyFile, SR);

  try
    while iCode = 0 do
    begin
      if (SR.Name[1] <> '.') and (SR.Attr <> faVolumeID) then
      begin
        if (SR.Attr and faDirectory = faDirectory) then
          Result := Result + GetFolderSize(sPath + SR.Name)
        else if (SR.Attr and faVolumeID <> faVolumeID) then
        begin
          Result := Result + SR.Size;
        end;
      end;
      iCode := SysUtils.FindNext(SR);
    end;
  finally
    SysUtils.FindClose(SR);
  end;

end;



{ stream utilities }


const
  INT_IDX    = 0;
  DOUBLE_IDX = 1;
  STR_IDX    = 2;
(*--------------------------------------------------------------------------------*)
class procedure Sys.WS(Stream: TStream; V: Integer);
var
  iType : integer;
begin
  iType := INT_IDX;
  Stream.WriteBuffer(iType, SizeOf(integer));
  Stream.WriteBuffer(V, SizeOf(integer));
end;
(*--------------------------------------------------------------------------------*)
class procedure Sys.WS(Stream: TStream; V: Double);
var
  iType : integer;
begin
  iType := DOUBLE_IDX;
  Stream.WriteBuffer(iType, SizeOf(integer));
  Stream.WriteBuffer(V, SizeOf(Double));
end;
(*--------------------------------------------------------------------------------*)
class procedure Sys.WS(Stream: TStream; V: String);
var
  Len   : integer;
  iType : integer;
begin
  iType := STR_IDX;
  Stream.WriteBuffer(iType, SizeOf(integer));
  Len := Length(V);
  Stream.WriteBuffer(Len, SizeOf(Len));
  Stream.WriteBuffer(PChar(V)^, Len * SizeOf(Char));
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.RS(Stream: TStream): Variant;

  function _AsInteger: integer;
  begin
    Result := 0;
    if (Stream.Size - Stream.Position) < SizeOf(Result) then Exit; //==>
    Stream.ReadBuffer(Result, SizeOf(Result));
  end;

  function _AsDouble: Double;
  begin
    Result := 0;
    if (Stream.Size - Stream.Position) < SizeOf(Result) then Exit; //==>
    Stream.ReadBuffer(Result, SizeOf(Result));
  end;

  function _AsString: string;
  var
    Len : integer;
  begin
    Len := SizeOf(Integer);

    Result := '';
    if (Stream.Size - Stream.Position) < SizeOf(Len) then Exit; //==>
    Stream.ReadBuffer(Len, SizeOf(Len));
    if (Stream.Size - Stream.Position) < Len then Exit; //==>

    SetString(Result, PChar(nil), Len);
    Stream.ReadBuffer(PChar(Result)^, Len * SizeOf(Char));
  end;

var
  iType : integer;
begin
  Result := 0;
  iType := 0;

  if (Stream.Size - Stream.Position) < SizeOf(integer) then Exit; //==>

  Stream.ReadBuffer(iType, SizeOf(integer));
  case iType of
    INT_IDX     : Result := _AsInteger;
    DOUBLE_IDX  : Result := _AsDouble;
    STR_IDX     : Result := _AsString;
    else  raise Exception.Create('Stream read error');
  end;
end;
(*----------------------------------------------------------------------------------*)
class procedure Sys.WSL(Stream : TStream; const List: TStrings);
var
  i : integer;
  Len : integer;
  iCount : integer;
begin
  iCount := List.Count;
  Stream.WriteBuffer(iCount, SizeOf(iCount));
  for i := 0 to List.Count - 1 do
  begin
    Len := Length(List[i]) * SizeOf(AnsiChar);
    Stream.WriteBuffer( Len, SizeOf(Len) );
    Stream.WriteBuffer( PChar(List[i])^, Len);
  end;
end;
(*----------------------------------------------------------------------------------*)
class procedure Sys.RSL(Stream : TStream;  List: TStrings);
var
  i : integer;
  iCount : integer;
  Buffer : string;
  Len : integer;
begin
  List.Clear;

  Len := 0;
  iCount := 0;
  Stream.ReadBuffer(iCount, SizeOf(iCount));

  for i := 0 to iCount - 1 do
  begin
    Buffer := '';
    try
      Stream.ReadBuffer(Len, SizeOf(Len));
      SetString(Buffer, (nil), Len);
      Stream.ReadBuffer(PAnsiString(Buffer)^, Len);
    except
      Buffer := '';
    end;
    List.Add(Buffer);
  end;
end;
{----------------------------------------------------------------------------------
 Description    : rounds D to have two digits after point so  2,24499 becomes 2,24
 Error checking : NO
 Notes          : if it is a monetary calculation - roundation then it should used
                  the currency data type instead of Double.
                  Currency stores 4 decimals not 2, so it will still needs to round off the
                  last 2 decimal digits you won't need to round due to repeating decimals in
                  the binary format of the number
----------------------------------------------------------------------------------}
class function  Sys.RoundToTwo(D: Double): Double;
begin
  Result := Trunc(100 * D + 0.5) / 100;
end;
(*----------------------------------------------------------------------------
 Description    : RoundTo rounds the Number to the desired number of Digits, where it is
                  rounded to the next higher absolut number. That is (Number = 88.84678
                  and Digits = 2):
                  88.84678 -> 88.85 and -88.84678 -> -88.85
                  If you eliminate the last line you get -88.84 (instead of -88.85) on
                  negative numbers.
 Author         : Wallner Christian <c.wallner@morocutti.com>
 Error checking : ?
 Examble        : Edit1.Text := FloatToStr(RoundTo(Periodic_Payment, 2));
 ----------------------------------------------------------------------------*)
class function  Sys.RoundToCustom(Number: Double; Digits: Integer = 2): Double;
var
  Exponent: Double;
begin
  Exponent := Exp(Digits * Ln(10));
  Result := Int(Abs(Number) * Exponent + 0.5) / Exponent;
  if Number < 0 then
    Result := -Result;
end;
(*----------------------------------------------------------------------------
 WARNING: Math.RoundTo() is defective.
 Try Number = 0,875 and Digits = 2. It returns 0,87 which is wrong.
 The RoundToCustom(), above, returns the correct result 0,88
 ----------------------------------------------------------------------------*)
class function  Sys.RoundTo(Number: Double; Digits: Integer = 2): Double;
begin
  Result := RoundTo(Number, -Digits);
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.IsWholeNumber(const D: Double): boolean;
const
  Delta = 1E-6;
begin
  //Result := Frac(V) = 0;
  Result := (Abs(Frac(D)) < Delta);
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.TwoComplement(Number: integer): integer;
begin
  Result := ((not Number) + 1);
end;
(*----------------------------------------------------------------------------
 ÅðéóôñÝöåé ôï ðïóïóôü ôïõ Numerator óôïí Denominator, ùò äåêáäéêü áñéèìü.
 ð.÷ Percent(3, 9) åðéóôñÝöåé 3.333
 ----------------------------------------------------------------------------*)
class function  Sys.Percent(const Numerator, Denominator: Double): Double;
begin
  if (Numerator = 0) or (Denominator = 0) then
    Result := 0
  else
    Result := (Numerator / Denominator) * 100;
end;
(*----------------------------------------------------------------------------
 ÅðéóôñÝöåé ôçí áîßá ðïõ áíôéðñïóùðåýåé ôï Percent óôï Value
 ð÷. PercenOf(120, 19) åðéóôñÝöåé 22.8
 ----------------------------------------------------------------------------*)
class function  Sys.PercentOf(const Value, aPercent: Double): Double;
begin
  Result := (Value * aPercent) / 100;
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.Average(const Numerator, Denominator: Double ): Double;
begin
  if (Numerator = 0) or (Denominator = 0) then
    Result := 0
  else Result := (Numerator / Denominator );
end;
(*----------------------------------------------------------------------------*)
{ Float mod function }
class function Sys.fmod(X, Y: Double): Double;
begin
  Result := Frac(X / Y) * Y;
end;

(*--------------------------------------------------------------------------------*)
class function  Sys.Min(const A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.Max(const A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.Min(const A, B: Double): Double;
begin
  if A < B then Result := A else Result := B;
end;
(*--------------------------------------------------------------------------------*)
class function  Sys.Max(const A, B: Double): Double;
begin
  if A > B then Result := A else Result := B;
end;

class function Sys.CreateSyncObject: ISyncObject;
begin
  Result := _TSyncObject.Create() as ISyncObject;
end;

{
class function  Sys.Min(const A, B: Extended): Extended;
begin
  if A < B then Result := A else Result := B;
end;

class function  Sys.Max(const A, B: Extended): Extended;
begin
  if A > B then Result := A else Result := B;
end;
}
class function Sys.InMainThread(): Boolean;
begin
   Result := GetCurrentThreadID() = MainThreadID;
end;

class procedure Sys.ProcessMessages();
begin
  if InMainThread and Assigned(ProcessMessagesMethod) then
    ProcessMessagesMethod()
  else
    Sleep(0);
end;

class procedure Sys.ClearObjectList(List: TList);
begin
  while (List.Count > 0) do
  begin
    try
      TObject(List[List.Count - 1]).Free();
    except
    end;
    List.Delete(List.Count - 1);
  end;
end;

{ TGenList }

function TGenList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TGenList.GetItem(Index: SizeInt): T;
begin
  Result := FItems[Index];
end;

function TGenList.GetItemList: specialize TArray<T>;
begin
  Result := FItems;
end;

procedure TGenList.SetItem(Index: SizeInt; Item: T);
begin
  FItems[Index] := Item;
end;

function TGenList.AdjustCapacityForItem(): SizeInt;
begin
  Result := Length(FItems);

  if (FCount < 4) and (Result < 4) then
    SetLength(FItems, 4)
  else if FCount = High(FCount) then
    OutOfMemoryError()
  else if FCount = Result then
    SetLength(FItems, Result * 2);

  Result := FCount;
  Inc(FCount);
end;

function TGenList.AdjustCapacityForRange(RangeCount: SizeInt): SizeInt;
begin
  if RangeCount < 0 then
    raise EArgumentOutOfRangeException.Create('Argument out of range');

  if RangeCount = 0 then
    Exit(FCount - 1);

  if (FCount = 0) and (Length(FItems) = 0) then
    SetLength(FItems, 4)
  else if FCount = High(FCount) then
    OutOfMemoryError();

  Result := Length(FItems);
  while Pred(FCount + RangeCount) >= Result do
  begin
    SetLength(FItems, Result * 2);
    Result := Length(FItems);
  end;

  Result := FCount;
  Inc(FCount, RangeCount);
end;

constructor TGenList.Create();
begin
  inherited Create();

end;

destructor TGenList.Destroy();
begin
  inherited Destroy();
end;

procedure TGenList.Clear();
begin

end;

function TGenList.Add(Item: T): Integer;
var
  pInfo: PTypeInfo;
begin
  FItems := Concat(FItems, [Item]);
  Result := Length(FItems);
  pInfo := TypeInfo(Item);
end;

procedure TGenList.Insert(Index: Integer; Item: T);
begin
  System.Insert([Item], FItems, Index);
end;

procedure TGenList.Remove(Item: T);
begin
  RemoveAt(IndexOf(Item));
end;

procedure TGenList.RemoveAt(Index: Integer);
begin
  System.Delete(FItems, Index, 1);
end;

procedure TGenList.AddRange(constref Range: array of T);
var
  Item: T;
begin
  for Item in Range do
    Add(Item);
end;

procedure TGenList.InsertRange(Index: SizeInt; constref Range: array of T);
var
  Item: T;
  i: SizeInt;
begin
  i := 0;
  for Item in Range do
  begin
    Insert(Index + i, Item);
    Inc(i);
  end;
end;

function TGenList.Contains(Item: T): Boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

function TGenList.IndexOf(Item: T): Integer;
var
  i : Integer;
begin
  for i := Low(FItems) to High(FItems) do
  begin
    if FItems[i] = Item then
      Exit(i);
  end;

  Exit(-1);
end;













end.

