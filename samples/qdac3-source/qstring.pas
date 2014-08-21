unit qstring;
{$I 'qdac.inc'}

interface

{
  本源码来自QDAC项目，版权归swish(QQ:109867294)所有。
  (1)、使用许可及限制
  您可以自由复制、分发、修改本源码，但您的修改应该反馈给作者，并允许作者在必要时，
  合并到本项目中以供使用，合并后的源码同样遵循QDAC版权声明限制。
  您的产品的关于中，应包含以下的版本声明:
  本产品使用的JSON解析器来自QDAC项目中的QJSON，版权归作者所有。
  (2)、技术支持
  有技术问题，您可以加入QDAC官方QQ群250530692共同探讨。
  (3)、赞助
  您可以自由使用本源码而不需要支付任何费用。如果您觉得本源码对您有帮助，您可以赞
  助本项目（非强制），以使作者不为生活所迫，有更多的精力为您呈现更好的作品：
  赞助方式：
  支付宝： guansonghuan@sina.com 姓名：管耸寰
  建设银行：
  户名：管耸寰
  账号：4367 4209 4324 0179 731
  开户行：建设银行长春团风储蓄所
}

{ 修订日志
  2014.8.15
  =========
  * 清理并验证了TQBytesCatHelper引起的2007编译无法通过的问题(秋风起逸以凉报告并验证)
  + PQStringA类型定义

  2014.8.14
  =========
  * 修正了TQBytesCatHelper.NeedSize函数在Delphi2007下无法编译的问题(音儿小白报告并提供修改)
  2014.8.5
  ========
  * BinToHex加入ALowerCase参数，以支持使用小写的十六进制表示方式
  2014.8.1
  =========
  + 新增函数SameCharsA/U/W计算相同的字符数，EndWithA/U/W判断是符以指定的字符串结尾
  2014.7.17
  =========
  + 新增BinaryCmp函数，用于等价于C中的memcmp函数
  2014.7.16
  =========
  + 新增MemScan函数用于在指定的内存区域中查找指定的字节序列
  2014.7.12
  =========
  * 修正了DecodeLineU中递归调用自己的错误(音儿小白报告)
  * 修正了CharCountU检查字符宽度时对双字节Utf8编码的检测错误
  2014.7.10
  =========
  + 新增以下函数：StringReplicateW,NameOfW,ValueOfW,IndexOfNameW,IndexOfValueW

  2014.6.26
  =========
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
  2014.6.21
  ==========
  * 修正了C++ Builder中编译的问题
  2014.6.19
  ==========
  * 修正了QuotedStr对于长度为0的字符串编码出错的问题
}
uses classes, sysutils{$IFDEF UNICODE}, rtti{$ENDIF}
{$IFDEF POSIX}
    , Posix.String_
{$ENDIF}
    ;
{$HPPEMIT '#pragma link "qstring"'}

type
{$IFDEF UNICODE}
  QStringW = UnicodeString;
  TValueArray = array of TValue;
{$ELSE}
  QStringW = WideString;
{$ENDIF UNICODE}
{$IF RTLVersion<25}
  IntPtr = Integer;
{$IFEND IntPtr}
  QCharA = Byte;
  QCharW = WideChar;
  PQCharA = ^QCharA;
  PPQCharA = ^PQCharA;
  PQStringA = ^QStringA;
  PQCharW = PWideChar;
  PPQCharW = ^PQCharW;
  PQStringW = ^QStringW;
  TTextEncoding = (teUnknown, { 未知的编码 }
    teAuto, { 自动检测 }
    teAnsi, { Ansi编码 }
    teUnicode16LE, { Unicode LE 编码 }
    teUnicode16BE, { Unicode BE 编码 }
    teUTF8 { UTF8编码 }
    );

  // 从A结尾的为Ansi编码支持的函数，以U结尾的是Utf8编码支持的函数，以W结尾的为UCS2
  QStringA = record
  private
    FValue: TBytes;
    function GetChars(AIndex: Integer): QCharA;
    procedure SetChars(AIndex: Integer; const Value: QCharA);
    function GetLength: Integer;
    procedure SetLength(const Value: Integer);
    function GetIsUtf8: Boolean;
  public
    class operator Implicit(const S: QStringW): QStringA;
    class operator Implicit(const S: QStringA): PQCharA;
    class operator Implicit(const S: QStringA): TBytes;
    class operator Implicit(const ABytes: TBytes): QStringA;
    class operator Implicit(const S: QStringA): QStringW;
    class operator Implicit(const S: PQCharA): QStringA;
    // 字符串比较
    procedure From(p: PQCharA; AOffset, ALen: Integer);
    property Chars[AIndex: Integer]: QCharA read GetChars
      write SetChars; default;
    property Length: Integer read GetLength write SetLength;
    property IsUtf8: Boolean read GetIsUtf8;
  end;

  // 字符串拼接类
  TQStringCatHelperW = class
  private
    FValue: array of QCharW;
    FStart, FDest: PQCharW;
    FBlockSize: Integer;
    FSize: Integer;
    function GetValue: QStringW;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    function GetChars(AIndex: Integer): QCharW;
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    function Cat(p: PQCharW; len: Integer): TQStringCatHelperW; overload;
    function Cat(const S: QStringW): TQStringCatHelperW; overload;
    function Cat(c: QCharW): TQStringCatHelperW; overload;
    function Cat(const V: Int64): TQStringCatHelperW; overload;
    function Cat(const V: Double): TQStringCatHelperW; overload;
    function Cat(const V: Boolean): TQStringCatHelperW; overload;
    function Cat(const V: Currency): TQStringCatHelperW; overload;
    function Cat(const V: TGuid): TQStringCatHelperW; overload;
    function Cat(const V: Variant): TQStringCatHelperW; overload;
    function Replicate(const S: QStringW; count: Integer): TQStringCatHelperW;
    function Back(ALen: Integer): TQStringCatHelperW;
    function BackIf(const S: PQCharW): TQStringCatHelperW;
    procedure Reset;
    property Value: QStringW read GetValue;
    property Chars[AIndex: Integer]: QCharW read GetChars;
    property Start: PQCharW read FStart;
    property Current: PQCharW read FDest;
    property Position: Integer read GetPosition write SetPosition;
  end;

  TQBytesCatHelper = class
  private
    FValue: TBytes;
    FStart, FDest: PByte;
    FBlockSize: Integer;
    FSize: Integer;
    function GetBytes(AIndex: Integer): Byte;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    procedure SetCapacity(const Value: Integer);
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    function Cat(const V: Byte): TQBytesCatHelper; overload;
    function Cat(const V: Shortint): TQBytesCatHelper; overload;
    function Cat(const V: Word): TQBytesCatHelper; overload;
    function Cat(const V: Smallint): TQBytesCatHelper; overload;
    function Cat(const V: Cardinal): TQBytesCatHelper; overload;
    function Cat(const V: Integer): TQBytesCatHelper; overload;
    function Cat(const V: Int64): TQBytesCatHelper; overload;
{$IFNDEF NEXTGEN}
    function Cat(const V: AnsiChar): TQBytesCatHelper; overload;
    function Cat(const V: AnsiString): TQBytesCatHelper; overload;
{$ENDIF}
    function Cat(const V: QStringA; ACStyle: Boolean = False)
      : TQBytesCatHelper; overload;
    function Cat(const c: QCharW): TQBytesCatHelper; overload;
    function Cat(const S: QStringW): TQBytesCatHelper; overload;
    function Cat(const ABytes: TBytes): TQBytesCatHelper; overload;
    function Cat(const AData: Pointer; const ALen: Integer)
      : TQBytesCatHelper; overload;
    function Cat(const V: Single): TQBytesCatHelper; overload;
    function Cat(const V: Double): TQBytesCatHelper; overload;
    function Cat(const V: Boolean): TQBytesCatHelper; overload;
    function Cat(const V: Currency): TQBytesCatHelper; overload;
    function Cat(const V: TGuid): TQBytesCatHelper; overload;
    function Cat(const V: Variant): TQBytesCatHelper; overload;
    function Replicate(const ABytes: TBytes; ACount: Integer): TQBytesCatHelper;
    function Back(ALen: Integer): TQBytesCatHelper;
    procedure Reset;
    property Value: TBytes read FValue;
    property Bytes[AIndex: Integer]: Byte read GetBytes;
    property Start: PByte read FStart;
    property Current: PByte read FDest;
    property Position: Integer read GetPosition write SetPosition;
    property Capacity: Integer read FSize write SetCapacity;
  end;

  IQPtr = interface(IInterface)
    function Get: Pointer;
  end;

  TQPtr = class(TInterfacedObject, IQPtr)
  private
    FObject: TObject;
  public
    constructor Create(AObject: TObject); overload;
    destructor Destroy; override;
    class function Bind(AObject: TObject): IQPtr;
    function Get: Pointer;
  end;

  // UTF8编码与Unicode编码转换函数，使用自己的实现
function Utf8Decode(p: PQCharA; l: Integer): QStringW; overload;
function Utf8Decode(const p: QStringA): QStringW; overload;
function Utf8Encode(p: PQCharW; l: Integer): QStringA; overload;
function Utf8Encode(const p: QStringW): QStringA; overload;
// Ansi编码与Unicode编码转换函数，使用系统的TEncoding实现
function AnsiEncode(p: PQCharW; l: Integer): QStringA; overload;
function AnsiEncode(const p: QStringW): QStringA; overload;
function AnsiDecode(p: PQCharA; l: Integer): QStringW; overload;
function AnsiDecode(const p: QStringA): QStringW; overload;

function CNSpellChars(S: QStringA; AIgnoreEnChars: Boolean): QStringW; overload;
function CNSpellChars(S: QStringW; AIgnoreEnChars: Boolean): QStringW; overload;

// 计算当前字符的长度
function CharSizeA(c: PQCharA): Integer;
function CharSizeU(c: PQCharA): Integer;
function CharSizeW(c: PQCharW): Integer;
// 计算字符数函数，CharCountW重写以包括UCS2扩展区字符处理
function CharCountA(const source: QStringA): Integer;
function CharCountW(const S: QStringW): Integer;
function CharCountU(const source: QStringA): Integer;
// 检查字符是否在指定的列表中
function CharInA(const c: PQCharA; const list: array of QCharA;
  ACharLen: PInteger = nil): Boolean;
function CharInW(const c: PQCharW; const list: array of QCharW;
  ACharLen: PInteger = nil): Boolean; overload;
function CharInW(const c, list: PQCharW; ACharLen: PInteger = nil)
  : Boolean; overload;
function CharInU(const c: PQCharA; const list: array of QCharA;
  ACharLen: PInteger = nil): Boolean;

// 检查是否是空白字符
function IsSpaceA(const c: PQCharA; ASpaceSize: PInteger = nil): Boolean;
function IsSpaceW(const c: PQCharW; ASpaceSize: PInteger = nil): Boolean;
function IsSpaceU(const c: PQCharA; ASpaceSize: PInteger = nil): Boolean;

// 全角半角转换
function CNFullToHalf(const S: QStringW): QStringW;
function CNHalfToFull(const S: QStringW): QStringW;

// 引号处理
function QuotedStrA(const S: QStringA; const AQuoter: QCharA = $27): QStringA;
function QuotedStrW(const S: QStringW; const AQuoter: QCharW = #$27): QStringW;
function SQLQuoted(const S: QStringW): QStringW;
function DequotedStrA(const S: QStringA; const AQuoter: QCharA = $27): QStringA;
function DequotedStrW(const S: QStringW; const AQuoter: QCharW = #$27)
  : QStringW;

// 跳过列表中的字符
function SkipCharA(var p: PQCharA; const list: array of QCharA): Integer;
function SkipCharU(var p: PQCharA; const list: array of QCharA): Integer;
function SkipCharW(var p: PQCharW; const list: array of QCharA)
  : Integer; overload;
function SkipCharW(var p: PQCharW; const list: PQCharW): Integer; overload;

// 跳过空白字符，对于 Ansi编码，跳过的是#9#10#13#161#161，对于UCS编码，跳过的是#9#10#13#$3000
function SkipSpaceA(var p: PQCharA): Integer;
function SkipSpaceU(var p: PQCharA): Integer;
function SkipSpaceW(var p: PQCharW): Integer;
// 跳过一行,以#10为行结尾
function SkipLineA(var p: PQCharA): Integer;
function SkipLineU(var p: PQCharA): Integer;
function SkipLineW(var p: PQCharW): Integer;
// 跳过直接遇到指定的字符
function SkipUntilA(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA = 0): Integer;
function SkipUntilU(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA = 0): Integer;
function SkipUntilW(var p: PQCharW; AExpects: array of QCharW;
  AQuoter: QCharW = #0): Integer; overload;
function SkipUntilW(var p: PQCharW; AExpects: PQCharW; AQuoter: QCharW = #0)
  : Integer; overload;
// 查找字符所在行列号，返回行的起始地址
function StrPosA(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
function StrPosU(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
function StrPosW(Start, Current: PQCharW; var ACol, ARow: Integer): PQCharW;

// 字符串分解
function DecodeTokenA(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
function DecodeTokenU(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
function DecodeTokenW(var p: PQCharW; ADelimiters: array of QCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): QStringW; overload;
function DecodeTokenW(var p: PQCharW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreSpace: Boolean): QStringW; overload;
function DecodeTokenW(var S: QStringW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreCase, ARemove: Boolean): QStringW; overload;
function LeftStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
function RightStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
function StrBetween(var S: PQCharW; AStartTag, AEndTag: QStringW;
  AIgnoreCase: Boolean): QStringW;
function TokenWithIndex(var S: PQCharW; AIndex: Integer; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSapce: Boolean): QStringW;

// 获取一行
function DecodeLineA(var p: PQCharA; ASkipEmpty: Boolean = True): QStringA;
function DecodeLineU(var p: PQCharA; ASkipEmpty: Boolean = True): QStringA;
function DecodeLineW(var p: PQCharW; ASkipEmpty: Boolean = True): QStringW;

// 判断是否是以指定的字符串开始
function StartWithA(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
function StartWithU(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
function StartWithW(S, startby: PQCharW; AIgnoreCase: Boolean): Boolean;
function EndWithA(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
function EndWithU(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
function EndWithW(const S, endby: QStringW; AIgnoreCase: Boolean): Boolean;
function SameCharsA(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
function SameCharsU(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
function SameCharsW(s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
// 加载文本
function LoadTextA(AFileName: String; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextA(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextU(AFileName: String; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextU(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringA; overload;
function LoadTextW(AFileName: String; AEncoding: TTextEncoding = teUnknown)
  : QStringW; overload;
function LoadTextW(AStream: TStream; AEncoding: TTextEncoding = teUnknown)
  : QStringW; overload;

// 保存文本
procedure SaveTextA(AFileName: String; const S: QStringA); overload;
procedure SaveTextA(AStream: TStream; const S: QStringA); overload;
procedure SaveTextU(AFileName: String; const S: QStringA;
  AWriteBom: Boolean = True); overload;
procedure SaveTextU(AStream: TStream; const S: QStringA;
  AWriteBom: Boolean = True); overload;
procedure SaveTextW(AFileName: String; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextW(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean = True); overload;
procedure SaveTextWBE(AStream: TStream; const S: QStringW;
  AWriteBom: Boolean = True); overload;

// 字符串中查找子串，UTF8直接使用Ansi版本即可
function StrStrA(s1, s2: PQCharA): PQCharA;
function StrIStrA(s1, s2: PQCharA): PQCharA;
function StrStrU(s1, s2: PQCharA): PQCharA;
function StrIStrU(s1, s2: PQCharA): PQCharA;
function StrStrW(s1, s2: PQCharW): PQCharW;
function StrIStrW(s1, s2: PQCharW): PQCharW;
function StrDupX(const S: PQCharW; ACount: Integer): QStringW;
function StrDupW(const S: PQCharW; AOffset: Integer = 0;
  const ACount: Integer = MaxInt): QStringW;
function StrCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
function StrNCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean;
  ALength: Integer): Integer;
function IsHexChar(c: QCharW): Boolean; inline;
function HexValue(c: QCharW): Integer;
function HexChar(V: Byte): QCharW;
function TryStrToGuid(const S: QStringW; var AGuid: TGuid): Boolean;
function StringReplaceW(const S, Old, New: QStringW; AFlags: TReplaceFlags)
  : QStringW;
function StringReplicateW(const S: QStringW; ACount: Integer): QStringW;
function MemScan(S: Pointer; len_s: Integer; sub: Pointer;
  len_sub: Integer): Pointer;
function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
// 下面的函数只能Unicode版本，没有Ansi和UTF-8版本，如果需要，再加入
function NameOfW(const S: QStringW; ASpliter: QCharW): QStringW;
function ValueOfW(const S: QStringW; ASpliter: QCharW): QStringW;
function IndexOfNameW(AList: TStrings; const AName: QStringW;
  ASpliter: QCharW): Integer;
function IndexOfValueW(AList: TStrings; const AValue: QStringW;
  ASpliter: QCharW): Integer;
function DeleteCharW(const ASource, ADeletes: QStringW): QStringW;
function ContainsCharW(const S, ACharList: QStringW): Boolean;
function HtmlEscape(const S: QStringW): QStringW;
function HtmlUnescape(const S: QStringW): QStringW;
function HtmlTrimText(const S: QStringW): QStringW;
function LeftStrCount(const S: QStringW; const sub: QStringW): Integer;
function RightStrCount(const S: QStringW; const sub: QStringW): Integer;
// 下面是一些辅助函数
function ParseInt(var S: PQCharW; var ANum: Int64): Integer;
function ParseHex(var p: PQCharW; var Value: Int64): Integer;
function ParseNumeric(var S: PQCharW; var ANum: Extended): Boolean;
function ParseDateTime(S: PWideChar; var AResult: TDateTime): Boolean;
function ParseWebTime(p: PWideChar; var AResult: TDateTime): Boolean;
function RollupSize(ASize: Int64): QStringW;
function RollupTime(ASeconds: Int64): QStringW;
function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean)
  : TTextEncoding;
procedure ExchangeByteOrder(p: PQCharA; l: Integer); overload;
function ExchangeByteOrder(V: Smallint): Smallint; overload; inline;
function ExchangeByteOrder(V: Word): Word; overload; inline;
function ExchangeByteOrder(V: Integer): Integer; overload; inline;
function ExchangeByteOrder(V: Cardinal): Cardinal; overload; inline;
function ExchangeByteOrder(V: Int64): Int64; overload; inline;
function ExchangeByteOrder(V: Single): Single; overload; inline;
function ExchangeByteOrder(V: Double): Double; overload; inline;

procedure FreeObject(AObject: TObject); inline;
// 原子操作函数
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer;
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer;
{$IF RTLVersion<26}
// 为与XE6兼容，InterlockedCompareExchange等价
function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; inline;
// 等价于InterlockedExchanged
function AtomicExchange(var Target: Integer; Value: Integer): Integer; inline;
function AtomicIncrement(var Target: Integer): Integer; inline;
function AtomicDecrement(var Target: Integer): Integer; inline;
{$IFEND <XE5}
//
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean = False)
  : QStringW; overload;
function BinToHex(const ABytes: TBytes; ALowerCase: Boolean = False)
  : QStringW; overload;
function HexToBin(const S: QStringW): TBytes; overload;
procedure HexToBin(const S: QStringW; var AResult: TBytes); overload;
// 计算指定内容的哈希值
function HashOf(const p: Pointer; l: Integer): Cardinal;

var
  JavaFormatUtf8: Boolean;

const
  SLineBreak: PQCharW = {$IFDEF MSWINDOWS}#13#10{$ELSE}#10{$ENDIF};

implementation

uses dateutils, math, variants
{$IFDEF MSWINDOWS}
    , windows
{$ENDIF}
{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
    , AnsiStrings
{$IFEND >=XE5}
    ;

resourcestring
  SBadUnicodeChar = '无效的Unicode字符:%d';
  SBadUtf8Char = '无效的UTF8字符:%d';
  SOutOfIndex = '索引越界，值 %d 不在[%d..%d]的范围内。';
  SDayName = '天';
  SHourName = '小时';
  SMinuteName = '分钟';
  SSecondName = '秒';

type
  TGBKCharSpell = record
    SpellChar: QCharW;
    StartChar, EndChar: Word;
  end;
{$IFDEF UNICODE}

  TIntArray = TArray<Integer>;
{$ELSE}
  TIntArray = array of Integer;
{$ENDIF}
{$IFDEF MSWINDOWS}
  TMSVCStrStr = function(s1, s2: PQCharA): PQCharA; cdecl;
  TMSVCStrStrW = function(s1, s2: PQCharW): PQCharW; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;
{$ENDIF}

var
  // GBK汉字拼音首字母表
  GBKSpells: array [0 .. 22] of TGBKCharSpell = (
    (
      SpellChar: 'A'; StartChar: $B0A1; EndChar: $B0C4;
    ), (SpellChar: 'B'; StartChar: $B0C5; EndChar: $B2C0;
    ), (SpellChar: 'C'; StartChar: $B2C1; EndChar: $B4ED;
    ), (SpellChar: 'D'; StartChar: $B4EE; EndChar: $B6E9;
    ), (SpellChar: 'E'; StartChar: $B6EA; EndChar: $B7A1;
    ), (SpellChar: 'F'; StartChar: $B7A2; EndChar: $B8C0;
    ), (SpellChar: 'G'; StartChar: $B8C1; EndChar: $B9FD;
    ), (SpellChar: 'H'; StartChar: $B9FE; EndChar: $BBF6;
    ), (SpellChar: 'J'; StartChar: $BBF7; EndChar: $BFA5;
    ), (SpellChar: 'K'; StartChar: $BFA6; EndChar: $C0AB;
    ), (SpellChar: 'L'; StartChar: $C0AC; EndChar: $C2E7;
    ), (SpellChar: 'M'; StartChar: $C2E8; EndChar: $C4C2;
    ), (SpellChar: 'N'; StartChar: $C4C3; EndChar: $C5B5;
    ), (SpellChar: 'O'; StartChar: $C5B6; EndChar: $C5BD;
    ), (SpellChar: 'P'; StartChar: $C5BE; EndChar: $C6D9;
    ), (SpellChar: 'Q'; StartChar: $C6DA; EndChar: $C8BA;
    ), (SpellChar: 'R'; StartChar: $C8BB; EndChar: $C8F5;
    ), (SpellChar: 'S'; StartChar: $C8F6; EndChar: $CBF0;
    ), (SpellChar: 'T'; StartChar: $CBFA; EndChar: $CDD9;
    ), (SpellChar: 'W'; StartChar: $CDDA; EndChar: $CEF3;
    ), (SpellChar: 'X'; StartChar: $CEF4; EndChar: $D188;
    ), (SpellChar: 'Y'; StartChar: $D1B9; EndChar: $D4D0;
    ), (SpellChar: 'Z'; StartChar: $D4D1; EndChar: $D7F9;));
{$IFDEF MSWINDOWS}
  hMsvcrtl: HMODULE;
  VCStrStr: TMSVCStrStr;
  VCStrStrW: TMSVCStrStrW;
  VCMemCmp: TMSVCMemCmp;
{$ENDIF}

const
  HtmlEscapeChars: array [0 .. 183] of QStringW = (QCharW(32), '&nbsp;',
    QCharW(34), '&quot;', QCharW(38), '&amp;', QCharW(60), '&lt;', QCharW(62),
    '&gt;', QCharW(161), '&iexcl;', QCharW(162), '&cent;', QCharW(163),
    '&pound;', QCharW(164), '&curren;', QCharW(165), '&yen;', QCharW(166),
    '&brvbar;', QCharW(167), '&sect;', QCharW(168), '&uml;', QCharW(169),
    '&copy;', QCharW(170), '&ordf;', QCharW(171), '&laquo;', QCharW(172),
    '&not;', QCharW(173), '&shy;', QCharW(174), '&reg;', QCharW(175), '&macr;',
    QCharW(176), '&deg;', QCharW(177), '&plusmn;', QCharW(180), '&acute;',
    QCharW(181), '&micro;', QCharW(182), '&para;', QCharW(183), '&middot;',
    QCharW(184), '&cedil;', QCharW(186), '&ordm;', QCharW(187), '&raquo;',
    QCharW(191), '&iquest;', QCharW(192), '&Agrave;', QCharW(193), '&Aacute;',
    QCharW(194), '&circ;', QCharW(195), '&Atilde;', QCharW(197), '&ring;',
    QCharW(198), '&AElig;', QCharW(199), '&Ccedil;', QCharW(200), '&Egrave;',
    QCharW(201), '&Eacute;', QCharW(202), '&Ecirc;', QCharW(203), '&Euml;',
    QCharW(204), '&Igrave;', QCharW(205), '&Iacute;', QCharW(206), '&Icirc;',
    QCharW(207), '&Iuml;', QCharW(208), '&ETH;', QCharW(209), '&Ntilde;',
    QCharW(210), '&Ograve;', QCharW(211), '&Oacute;', QCharW(212), '&Ocirc;',
    QCharW(213), '&Otilde;', QCharW(214), '&Ouml;', QCharW(215), '&times;',
    QCharW(216), '&Oslash;', QCharW(217), '&Ugrave;', QCharW(218), '&Uacute;',
    QCharW(219), '&Ucirc;', QCharW(220), '&Uuml;', QCharW(221), '&Yacute;',
    QCharW(222), '&THORN;', QCharW(223), '&szlig;', QCharW(224), '&agrave;',
    QCharW(225), '&aacute;', QCharW(227), '&atilde;', QCharW(228), '&auml;',
    QCharW(229), '&aring;', QCharW(230), '&aelig;', QCharW(231), '&ccedil;',
    QCharW(232), '&egrave;', QCharW(233), '&eacute;', QCharW(234), '&ecirc;',
    QCharW(235), '&euml;', QCharW(236), '&igrave;', QCharW(237), '&iacute;',
    QCharW(238), '&icirc;', QCharW(239), '&iuml;', QCharW(240), '&ieth;',
    QCharW(241), '&ntilde;', QCharW(242), '&ograve;', QCharW(243), '&oacute;',
    QCharW(244), '&ocirc;', QCharW(245), '&otilde;', QCharW(246), '&ouml;',
    QCharW(247), '&divide;', QCharW(248), '&oslash;', QCharW(249), '&ugrave;',
    QCharW(250), '&uacute;', QCharW(251), '&ucirc;', QCharW(252), '&uuml;',
    QCharW(253), '&yacute;', QCharW(254), '&thorn;', QCharW(255), '&yuml;');
  // QString函数

function Utf8Decode(const p: QStringA): QStringW;
begin
if p.IsUtf8 then
  Result := Utf8Decode(PQCharA(p), p.Length)
else
  Result := AnsiDecode(p);
end;

function Utf8Encode(const p: QStringW): QStringA;
begin
Result := Utf8Encode(PQCharW(p), Length(p));
end;

function Utf8Decode(p: PQCharA; l: Integer): QStringW;
var
  ps, pe: PByte;
  pd, pds: PWord;
  c: Cardinal;
begin
if l <= 0 then
  begin
  ps := PByte(p);
  while ps^ <> 0 do
    Inc(ps);
  l := IntPtr(ps) - IntPtr(p);
  end;
ps := PByte(p);
pe := ps;
Inc(pe, l);
System.SetLength(Result, l);
pd := PWord(PQCharW(Result));
pds := pd;
while IntPtr(ps) < IntPtr(pe) do
  begin
  if (ps^ and $80) <> 0 then
    begin
    if (ps^ and $FC) = $FC then // 4000000+
      begin
      c := (ps^ and $03) shl 30;
      Inc(ps);
      c := c or ((ps^ and $3F) shl 24);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 18);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 12);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 6);
      Inc(ps);
      c := c or (ps^ and $3F);
      Inc(ps);
      c := c - $10000;
      pd^ := $D800 + ((c shr 10) and $3FF);
      Inc(pd);
      pd^ := $DC00 + (c and $3FF);
      Inc(pd);
      end
    else if (ps^ and $F8) = $F8 then // 200000-3FFFFFF
      begin
      c := (ps^ and $07) shl 24;
      Inc(ps);
      c := c or ((ps^ and $3F) shl 18);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 12);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 6);
      Inc(ps);
      c := c or (ps^ and $3F);
      Inc(ps);
      c := c - $10000;
      pd^ := $D800 + ((c shr 10) and $3FF);
      Inc(pd);
      pd^ := $DC00 + (c and $3FF);
      Inc(pd);
      end
    else if (ps^ and $F0) = $F0 then // 10000-1FFFFF
      begin
      c := (ps^ and $0F) shr 18;
      Inc(ps);
      c := c or ((ps^ and $3F) shl 12);
      Inc(ps);
      c := c or ((ps^ and $3F) shl 6);
      Inc(ps);
      c := c or (ps^ and $3F);
      Inc(ps);
      c := c - $10000;
      pd^ := $D800 + ((c shr 10) and $3FF);
      Inc(pd);
      pd^ := $DC00 + (c and $3FF);
      Inc(pd);
      end
    else if (ps^ and $E0) = $E0 then // 800-FFFF
      begin
      c := (ps^ and $1F) shl 12;
      Inc(ps);
      c := c or ((ps^ and $3F) shl 6);
      Inc(ps);
      c := c or (ps^ and $3F);
      Inc(ps);
      pd^ := c;
      Inc(pd);
      end
    else if (ps^ and $C0) = $C0 then // 80-7FF
      begin
      pd^ := (ps^ and $3F) shl 6;
      Inc(ps);
      pd^ := pd^ or (ps^ and $3F);
      Inc(pd);
      Inc(ps);
      end
    else
      raise Exception.Create(Format(SBadUtf8Char, [IntPtr(ps^)]));
    end
  else
    begin
    pd^ := ps^;
    Inc(ps);
    Inc(pd);
    end;
  end;
System.SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
end;

function Utf8Encode(p: PQCharW; l: Integer): QStringA;
var
  ps: PQCharW;
  pd, pds: PQCharA;
  c: Cardinal;
begin
if p = nil then
  Result.Length := 0
else
  begin
  if l <= 0 then
    begin
    ps := p;
    while ps^ <> #0 do
      Inc(ps);
    l := ps - p;
    end;
  Result.Length := l * 6; // UTF8每个字符最多6字节长,一次性分配足够的空间
  if l > 0 then
    begin
    Result.FValue[0] := 1;
    ps := p;
    pd := PQCharA(Result);
    pds := pd;
    while l > 0 do
      begin
      c := Cardinal(ps^);
      Inc(ps);
      if (c >= $D800) and (c <= $DFFF) then // Unicode 扩展区字符
        begin
        c := (c - $D800);
        if (ps^ >= #$DC00) and (ps^ <= #$DFFF) then
          begin
          c := $10000 + ((c shl 10) + (Cardinal(ps^) - $DC00));
          Inc(ps);
          Dec(l);
          end
        else
          raise Exception.Create(Format(SBadUnicodeChar, [IntPtr(ps^)]));
        end;
      Dec(l);
      if c = $0 then
        begin
        if JavaFormatUtf8 then // 按照Java格式编码，将#$0字符编码为#$C080
          begin
          pd^ := $C0;
          Inc(pd);
          pd^ := $80;
          Inc(pd);
          end
        else
          begin
          pd^ := c;
          Inc(pd);
          end;
        end
      else if c <= $7F then // 1B
        begin
        pd^ := c;
        Inc(pd);
        end
      else if c <= $7FF then // $80-$7FF,2B
        begin
        pd^ := $C0 or (c shr 6);
        Inc(pd);
        pd^ := $80 or (c and $3F);
        Inc(pd);
        end
      else if c <= $FFFF then // $8000 - $FFFF,3B
        begin
        pd^ := $E0 or (c shr 12);
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F);
        Inc(pd);
        pd^ := $80 or (c and $3F);
        Inc(pd);
        end
      else if c <= $1FFFFF then // $01 0000-$1F FFFF,4B
        begin
        pd^ := $F0 or (c shr 18); // 1111 0xxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
        end
      else if c <= $3FFFFFF then // $20 0000 - $3FF FFFF,5B
        begin
        pd^ := $F8 or (c shr 24); // 1111 10xx
        Inc(pd);
        pd^ := $F0 or ((c shr 18) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
        end
      else if c <= $7FFFFFFF then // $0400 0000-$7FFF FFFF,6B
        begin
        pd^ := $FC or (c shr 30); // 1111 11xx
        Inc(pd);
        pd^ := $F8 or ((c shr 24) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $F0 or ((c shr 18) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 12) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or ((c shr 6) and $3F); // 10 xxxxxx
        Inc(pd);
        pd^ := $80 or (c and $3F); // 10 xxxxxx
        Inc(pd);
        end;
      end;
    pd^ := 0;
    Result.Length := IntPtr(pd) - IntPtr(pds);
    end;
  end;
end;

function AnsiEncode(p: PQCharW; l: Integer): QStringA;
var
  ps: PQCharW;
begin
if l <= 0 then
  begin
  ps := p;
  while ps^ <> #0 do
    Inc(ps);
  l := ps - p;
  end;
if l > 0 then
  begin
{$IFDEF MSWINDOWS}
  Result.Length := WideCharToMultiByte(CP_ACP, 0, p, l, nil, 0, nil, nil);
  WideCharToMultiByte(CP_ACP, 0, p, l, PAnsiChar(PQCharA(Result)),
    Result.Length, nil, nil);
{$ELSE}
  Result.Length := l shl 1;
  Result.FValue[0] := 0;
  Move(p^, PQCharA(Result)^, l shl 1);
  Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.ANSI, Result.FValue,
    1, l shl 1);
{$ENDIF}
  end
else
  Result.Length := 0;
end;

function AnsiEncode(const p: QStringW): QStringA;
begin
Result := AnsiEncode(PQCharW(p), Length(p));
end;

function AnsiDecode(p: PQCharA; l: Integer): QStringW;
var
  ps: PQCharA;
{$IFNDEF MSWINDOWS}
  ABytes: TBytes;
{$ENDIF}
begin
if l <= 0 then
  begin
  ps := p;
  while ps^ <> 0 do
    Inc(ps);
  l := IntPtr(ps) - IntPtr(p);
  end;
if l > 0 then
  begin
{$IFDEF MSWINDOWS}
  System.SetLength(Result, MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p),
    l, nil, 0));
  MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p), l, PWideChar(Result),
    Length(Result));
{$ELSE}
  System.SetLength(ABytes, l);
  Move(p^, PByte(@ABytes[0])^, l);
  Result := TEncoding.ANSI.GetString(ABytes);
{$ENDIF}
  end
else
  System.SetLength(Result, 0);
end;

function AnsiDecode(const p: QStringA): QStringW;
begin
if p.IsUtf8 then
  Result := Utf8Decode(p)
else
{$IFDEF MSWINDOWS}
  Result := AnsiDecode(PQCharA(p), p.Length);
{$ELSE}
  Result := TEncoding.ANSI.GetString(p.FValue, 1, p.Length);
{$ENDIF}
end;

function CNSpellChars(S: QStringA; AIgnoreEnChars: Boolean): QStringW;
var
  p: PQCharA;
  pd, pds: PQCharW;
  function SpellOfChar: QCharW;
  var
    I: Integer;
    w: Word;
  begin
  w := p^ shl 8;
  Inc(p);
  w := w or p^;
  Inc(p);
  Result := #0;
  for I := 0 to 22 do
    begin
    if (w >= GBKSpells[I].StartChar) and (w <= GBKSpells[I].EndChar) then
      begin
      Result := GBKSpells[I].SpellChar;
      Break;
      end;
    end;
  end;

begin
if S.Length > 0 then
  begin
  p := PQCharA(S);
  System.SetLength(Result, S.Length);
  pd := PQCharW(Result);
  pds := pd;
  while p^ <> 0 do
    begin
    if p^ in [1 .. 127] then
      begin
      if not AIgnoreEnChars then
        begin
        pd^ := QCharW(p^);
        Inc(pd);
        end;
      Inc(p);
      end
    else
      begin
      pd^ := SpellOfChar;
      if pd^ <> #0 then
        Inc(pd);
      end;
    end;
  System.SetLength(Result, pd - pds);
  end
else
  System.SetLength(Result, 0);
end;

function CNSpellChars(S: QStringW; AIgnoreEnChars: Boolean): QStringW;
begin
Result := CNSpellChars(AnsiEncode(S), AIgnoreEnChars);
end;

function CharSizeA(c: PQCharA): Integer;
begin
{ GB18030,兼容GBK和GB2312
  单字节，其值从0到0x7F。
  双字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x40到0xFE（不包括0x7F）。
  四字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x30到0x39，第三个字节从0x81到0xFE，第四个字节从0x30到0x39。
}
{$IFDEF MSWINDOWS}
if GetACP = 936 then
{$ELSE}
if TEncoding.ANSI.CodePage = 936 then
{$ENDIF}
  begin
  Result := 1;
  if (c^ >= $81) and (c^ <= $FE) then
    begin
    Inc(c);
    if (c^ >= $40) and (c^ <= $FE) and (c^ <> $7F) then
      Result := 2
    else if (c^ >= $30) and (c^ <= $39) then
      begin
      Inc(c);
      if (c^ >= $81) and (c^ <= $FE) then
        begin
        Inc(c);
        if (c^ >= $30) and (c^ <= $39) then
          Result := 4;
        end;
      end;
    end;
  end
else
{$IFDEF QDAC_ANSISTRINGS}
  Result := AnsiStrings.StrCharLength(PAnsiChar(c));
{$ELSE}
{$IFDEF NEXTGEN}
  if TEncoding.ANSI.CodePage = CP_UTF8 then
    Result := CharSizeU(c)
  else if (c^ < 128) or (TEncoding.ANSI.CodePage = 437) then
    Result := 1
  else
    Result := 2;
{$ELSE}
{$IF RTLVersion>26}
  Result := AnsiStrings.StrCharLength(PAnsiChar(c));
{$ELSE}
  Result := sysutils.StrCharLength(PAnsiChar(c));
{$IFEND}
{$ENDIF}
{$ENDIF !QDAC_ANSISTRINGS}
end;

function CharSizeU(c: PQCharA): Integer;
begin
if (c^ and $80) = 0 then
  Result := 1
else
  begin
  if (c^ and $FC) = $FC then // 4000000+
    Result := 6
  else if (c^ and $F8) = $F8 then // 200000-3FFFFFF
    Result := 5
  else if (c^ and $F0) = $F0 then // 10000-1FFFFF
    Result := 4
  else if (c^ and $E0) = $E0 then // 800-FFFF
    Result := 3
  else if (c^ and $C0) = $C0 then // 80-7FF
    Result := 2
  else
    Result := 1;
  end
end;

function CharSizeW(c: PQCharW): Integer;
begin
if (c[0] >= #$DB00) and (c[0] <= #$DBFF) and (c[1] >= #$DC00) and
  (c[1] <= #$DFFF) then
  Result := 2
else
  Result := 1;
end;

function CharCountA(const source: QStringA): Integer;
var
  p: PQCharA;
  l, ASize: Integer;
begin
p := PQCharA(source);
l := source.Length;
Result := 0;
while l > 0 do
  begin
  ASize := CharSizeA(p);
  Dec(l, ASize);
  Inc(p, ASize);
  Inc(Result);
  end;
// Result:=TEncoding.ANSI.GetCharCount(source);
end;

function CharCountW(const S: QStringW): Integer;
var
  p, pe: PWord;
  ALen: Integer;
  procedure CountChar;
  begin
  if (p^ > $D800) and (p^ < $DFFF) then
    begin
    Inc(p);
    if (p^ >= $DC00) and (p^ < $DFFF) then
      begin
      Inc(p);
      Inc(Result);
      end
    else
      Result := -1;
    end
  else
    begin
    Inc(Result);
    Inc(p);
    end;
  end;

begin
Result := 0;
p := PWord(S);
ALen := Length(S);
pe := PWord(IntPtr(p) + (ALen shl 1));
while IntPtr(p) < IntPtr(pe) do
  CountChar;
end;

function CharCountU(const source: QStringA): Integer;
var
  p, pe: PQCharA;
  procedure CountChar;
  begin
  if (p^ and $80) = 0 then
    begin
    Inc(Result);
    Inc(p);
    end
  else if (p^ and $FC) = $FC then
    begin
    Inc(Result);
    Inc(p, 6);
    end
  else if (p^ and $F8) = $F8 then
    begin
    Inc(Result);
    Inc(p, 5);
    end
  else if (p^ and $F0) = $F0 then
    begin
    Inc(Result);
    Inc(p, 4);
    end
  else if (p^ and $E0) = $E0 then
    begin
    Inc(Result);
    Inc(p, 3);
    end
  else if (p^ and $C0) = $C0 then
    begin
    Inc(Result);
    Inc(p, 2);
    end
  else
    Result := -1;
  end;

begin
Result := 0;
p := PQCharA(source);
pe := PQCharA(IntPtr(p) + source.Length);
while (IntPtr(p) < IntPtr(pe)) and (Result >= 0) do
  CountChar;
end;

procedure CalcCharLengthA(var Lens: TIntArray; const list: array of QCharA);
var
  I, l: Integer;
begin
I := Low(list);
System.SetLength(Lens, Length(list));
while I <= High(list) do
  begin
  l := CharSizeA(@list[I]);
  Lens[I] := l;
  Inc(I, l);
  end;
end;

function CharInA(const c: PQCharA; const list: array of QCharA;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
count := High(list) + 1;
Result := False;
CalcCharLengthA(Lens, list);
I := Low(list);
while I < count do
  begin
  if CompareMem(c, @list[I], Lens[I]) then
    begin
    if ACharLen <> nil then
      ACharLen^ := Lens[I];
    Result := True;
    Break;
    end
  else
    Inc(I, Lens[I]);
  end;
end;

procedure CalcCharLengthW(var Lens: TIntArray; const list: array of QCharW);
var
  I, l: Integer;
begin
I := Low(list);
System.SetLength(Lens, Length(list));
while I <= High(list) do
  begin
  l := CharSizeW(@list[I]);
  Lens[I] := l;
  Inc(I, l);
  end;
end;

function CharInW(const c: PQCharW; const list: array of QCharW;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
count := High(list) + 1;
Result := False;
CalcCharLengthW(Lens, list);
I := Low(list);
while I < count do
  begin
  if c^ = list[I] then
    begin
    if Lens[I] = 2 then
      begin
      Result := c[1] = list[I + 1];
      if Assigned(ACharLen) and Result then
        ACharLen^ := 2;
      if Result then
        Break;
      end
    else
      begin
      Result := True;
      if Assigned(ACharLen) then
        ACharLen^ := 1;
      Break;
      end;
    end;
  Inc(I, Lens[I]);
  end;
end;

function CharInW(const c, list: PQCharW; ACharLen: PInteger): Boolean;
var
  p: PQCharW;
begin
Result := False;
p := list;
while p^ <> #0 do
  begin
  if p^ = c^ then
    begin
    if (p[0] >= #$DB00) and (p[0] <= #$DBFF) then
      begin
      // (p[1] >= #$DC00) and (p[1] <= #$DFFF)
      if p[1] = c[1] then
        begin
        Result := True;
        if ACharLen <> nil then
          ACharLen^ := 2;
        Break;
        end;
      end
    else
      begin
      Result := True;
      if ACharLen <> nil then
        ACharLen^ := 1;
      Break;
      end;
    end;
  Inc(p);
  end;
end;

procedure CalcCharLengthU(var Lens: TIntArray; const list: array of QCharA);
var
  I, l: Integer;
begin
I := Low(list);
System.SetLength(Lens, Length(list));
while I <= High(list) do
  begin
  l := CharSizeU(@list[I]);
  Lens[I] := l;
  Inc(I, l);
  end;
end;

function CharInU(const c: PQCharA; const list: array of QCharA;
  ACharLen: PInteger): Boolean;
var
  I, count: Integer;
  Lens: TIntArray;
begin
count := High(list) + 1;
Result := False;
CalcCharLengthU(Lens, list);
I := Low(list);
while I < count do
  begin
  if CompareMem(c, @list[I], Lens[I]) then
    begin
    if ACharLen <> nil then
      ACharLen^ := Lens[I];
    Result := True;
    Break;
    end
  else
    Inc(I, Lens[I]);
  end;
end;

function IsSpaceA(const c: PQCharA; ASpaceSize: PInteger): Boolean;
begin
if c^ in [9, 10, 13, 32] then
  begin
  Result := True;
  if Assigned(ASpaceSize) then
    ASpaceSize^ := 1;
  end
else if (c^ = 161) and (PQCharA(IntPtr(c) + 1)^ = 161) then
  begin
  Result := True;
  if Assigned(ASpaceSize) then
    ASpaceSize^ := 2;
  end
else
  Result := False;
end;

function IsSpaceW(const c: PQCharW; ASpaceSize: PInteger): Boolean;
begin
Result := (c^ = #9) or (c^ = #10) or (c^ = #13) or (c^ = #32) or (c^ = #$3000);
if Result and Assigned(ASpaceSize) then
  ASpaceSize^ := 1;
end;

function IsSpaceU(const c: PQCharA; ASpaceSize: PInteger): Boolean;
begin
// 全角空格$3000的UTF-8编码是227,128,128
if c^ in [9, 10, 13, 32] then
  begin
  Result := True;
  if Assigned(ASpaceSize) then
    ASpaceSize^ := 1;
  end
else if (c^ = 227) and (PQCharA(IntPtr(c) + 1)^ = 128) and
  (PQCharA(IntPtr(c) + 2)^ = 128) then
  begin
  Result := True;
  if Assigned(ASpaceSize) then
    ASpaceSize^ := 3;
  end
else
  Result := False;
end;

function CNFullToHalf(const S: QStringW): QStringW;
var
  p, pd: PWord;
  l: Integer;
begin
l := Length(S);
if l > 0 then
  begin
  System.SetLength(Result, l);
  p := PWord(PQCharW(S));
  pd := PWord(PQCharW(Result));
  while l > 0 do
    begin
    if (p^ = $3000) then // 全角空格'　'
      pd^ := $20
    else if (p^ >= $FF01) and (p^ <= $FF5E) then
      pd^ := $21 + (p^ - $FF01)
    else
      pd^ := p^;
    Dec(l);
    Inc(p);
    Inc(pd);
    end;
  end
else
  System.SetLength(Result, 0);
end;

function CNHalfToFull(const S: QStringW): QStringW;
var
  p, pd: PWord;
  l: Integer;
begin
l := Length(S);
if l > 0 then
  begin
  System.SetLength(Result, l);
  p := PWord(PQCharW(S));
  pd := PWord(PQCharW(Result));
  while l > 0 do
    begin
    if p^ = $20 then // 全角空格'　'
      pd^ := $3000
    else if (p^ >= $21) and (p^ <= $7E) then
      pd^ := $FF01 + (p^ - $21)
    else
      pd^ := p^;
    Dec(l);
    Inc(p);
    Inc(pd);
    end;
  end
else
  System.SetLength(Result, 0);
end;

function QuotedStrA(const S: QStringA; const AQuoter: QCharA): QStringA;
var
  p, pe, pd, pds: PQCharA;
begin
p := PQCharA(S);
Result.Length := S.Length shl 1;
pe := p;
Inc(pe, S.Length);
pd := PQCharA(Result);
pds := pd;
pd^ := AQuoter;
Inc(pd);
while IntPtr(p) < IntPtr(pe) do
  begin
  if p^ = AQuoter then
    begin
    pd^ := AQuoter;
    Inc(pd);
    pd^ := AQuoter;
    end
  else
    pd^ := p^;
  Inc(pd);
  Inc(p);
  end;
pd^ := AQuoter;
Result.Length := IntPtr(pd) - IntPtr(pds) + 1;
end;

function QuotedStrW(const S: QStringW; const AQuoter: QCharW): QStringW;
var
  p, pe, pd, pds: PQCharW;
  l: Integer;
begin
l := System.Length(S);
p := PQCharW(S);
SetLength(Result, (l + 1) shl 1);
pe := p;
Inc(pe, l);
pd := PQCharW(Result);
pds := pd;
pd^ := AQuoter;
Inc(pd);
while IntPtr(p) < IntPtr(pe) do
  begin
  if p^ = AQuoter then
    begin
    pd^ := AQuoter;
    Inc(pd);
    pd^ := AQuoter;
    end
  else
    pd^ := p^;
  Inc(pd);
  Inc(p);
  end;
pd^ := AQuoter;
SetLength(Result, ((IntPtr(pd) - IntPtr(pds)) shr 1) + 1);
end;

function SQLQuoted(const S: QStringW): QStringW;
begin
Result := QuotedStrW(S);
end;

function DequotedStrA(const S: QStringA; const AQuoter: QCharA): QStringA;
var
  p, pe, pd, pds: PQCharA;
begin
if (S.Length > 0) and (S[0] = AQuoter) and (S[S.Length - 1] = AQuoter) then
  begin
  p := PQCharA(S);
  pe := p;
  Inc(pe, S.Length);
  Inc(p);
  Result.Length := S.Length;
  pd := PQCharA(Result);
  pds := pd;
  while IntPtr(p) < IntPtr(pe) do
    begin
    if p^ = AQuoter then
      begin
      Inc(p);
      if p^ = AQuoter then
        begin
        pd^ := AQuoter;
        end
      else if IntPtr(p) < IntPtr(pe) then // 后面不是单引号,错误的字符串，直接拷贝内容
        begin
        pd^ := AQuoter;
        Inc(pd);
        pd^ := p^;
        end
      else
        Break;
      end
    else
      pd^ := p^;
    Inc(p);
    Inc(pd);
    end;
  Result.Length := IntPtr(pd) - IntPtr(pds);
  end
else
  Result := S;
end;

function DequotedStrW(const S: QStringW; const AQuoter: QCharW): QStringW;
var
  p, pe, pd, pds: PQCharW;
begin
if (Length(S) > 0) and (PQCharW(S)[0] = AQuoter) and
  (PQCharW(S)[Length(S) - 1] = AQuoter) then
  begin
  p := PQCharW(S);
  pe := p;
  Inc(pe, Length(S));
  Inc(p);
  SetLength(Result, Length(S));
  pd := PQCharW(Result);
  pds := pd;
  while IntPtr(p) < IntPtr(pe) do
    begin
    if p^ = AQuoter then
      begin
      Inc(p);
      if p^ = AQuoter then
        begin
        pd^ := AQuoter;
        end
      else if IntPtr(p) < IntPtr(pe) then // 后面不是单引号,错误的字符串，直接拷贝内容
        begin
        pd^ := AQuoter;
        Inc(pd);
        pd^ := p^;
        end
      else
        Break;
      end
    else
      pd^ := p^;
    Inc(p);
    Inc(pd);
    end;
  SetLength(Result, (IntPtr(pd) - IntPtr(pds)) shr 1);
  end
else
  Result := S;
end;

function SkipCharA(var p: PQCharA; const list: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharA;
begin
count := High(list) + 1;
Result := 0;
if count > 0 then
  begin
  CalcCharLengthA(Lens, list);
  ps := p;
  while p^ <> 0 do
    begin
    I := Low(list);
    AFound := False;
    while I < count do
      begin
      if CompareMem(p, @list[I], Lens[I]) then
        begin
        AFound := True;
        Inc(p, Lens[I]);
        Break;
        end
      else
        Inc(I, Lens[I]);
      end;
    if not AFound then
      begin
      Result := IntPtr(p) - IntPtr(ps);
      Break;
      end;
    end;
  end;
end;

function SkipCharU(var p: PQCharA; const list: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharA;
begin
count := High(list) + 1;
Result := 0;
if count > 0 then
  begin
  CalcCharLengthU(Lens, list);
  ps := p;
  while p^ <> 0 do
    begin
    I := Low(list);
    AFound := False;
    while I < count do
      begin
      if CompareMem(p, @list[I], Lens[I]) then
        begin
        AFound := True;
        Inc(p, Lens[I]);
        Break;
        end
      else
        Inc(I, Lens[I]);
      end;
    if not AFound then
      begin
      Result := IntPtr(p) - IntPtr(ps);
      Break;
      end;
    end;
  end;
end;

function SkipCharW(var p: PQCharW; const list: array of QCharA): Integer;
var
  I, count: Integer;
  Lens: TIntArray;
  AFound: Boolean;
  ps: PQCharW;
begin
count := High(list) + 1;
Result := 0;
if count > 0 then
  begin
  CalcCharLengthA(Lens, list);
  ps := p;
  while p^ <> #0 do
    begin
    I := Low(list);
    AFound := False;
    while I < count do
      begin
      if CompareMem(p, @list[I], Lens[I] shl 1) then
        begin
        AFound := True;
        Break;
        end
      else
        Inc(I, Lens[I]);
      end;
    if AFound then
      Inc(p)
    else
      begin
      Result := IntPtr(p) - IntPtr(ps);
      Break;
      end;
    end;
  end;
end;

function SkipCharW(var p: PQCharW; const list: PQCharW): Integer;
var
  l: Integer;
  ps: PQCharW;
begin
Result := 0;
if (list <> nil) and (list^ <> #0) then
  begin
  ps := p;
  while p^ <> #0 do
    begin
    if CharInW(p, list, @l) then
      Inc(p, l)
    else
      begin
      Result := IntPtr(p) - IntPtr(ps);
      Break;
      end;
    end;
  end;
end;

function SkipSpaceA(var p: PQCharA): Integer;
var
  ps: PQCharA;
  l: Integer;
begin
ps := p;
while p^ <> 0 do
  begin
  if IsSpaceA(p, @l) then
    Inc(p, l)
  else
    Break;
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceU(var p: PQCharA): Integer;
var
  ps: PQCharA;
  l: Integer;
begin
ps := p;
while p^ <> 0 do
  begin
  if IsSpaceU(p, @l) then
    Inc(p, l)
  else
    Break;
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceW(var p: PQCharW): Integer;
var
  ps: PQCharW;
  l: Integer;
begin
ps := p;
while p^ <> #0 do
  begin
  if IsSpaceW(p, @l) then
    Inc(p, l)
  else
    Break;
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

// 跳过一行,以#10为行结尾
function SkipLineA(var p: PQCharA): Integer;
var
  ps: PQCharA;
begin
ps := p;
while p^ <> 0 do
  begin
  if p^ = 10 then
    begin
    Inc(p);
    Break;
    end
  else
    Inc(p);
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipLineU(var p: PQCharA): Integer;
begin
Result := SkipLineA(p);
end;

function SkipLineW(var p: PQCharW): Integer;
var
  ps: PQCharW;
begin
ps := p;
while p^ <> #0 do
  begin
  if p^ = #10 then
    begin
    Inc(p);
    Break;
    end
  else
    Inc(p);
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function StrPosA(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
begin
ACol := 1;
ARow := 1;
Result := Start;
while IntPtr(Start) < IntPtr(Current) do
  begin
  if Start^ = 10 then
    begin
    Inc(ARow);
    ACol := 1;
    Inc(Start);
    Result := Start;
    end
  else
    begin
    Inc(Start, CharSizeA(Start));
    Inc(ACol);
    end;
  end;
end;

function StrPosU(Start, Current: PQCharA; var ACol, ARow: Integer): PQCharA;
begin
ACol := 1;
ARow := 1;
Result := Start;
while IntPtr(Start) < IntPtr(Current) do
  begin
  if Start^ = 10 then
    begin
    Inc(ARow);
    ACol := 1;
    Inc(Start);
    Result := Start;
    end
  else
    begin
    Inc(Start, CharSizeU(Start));
    Inc(ACol);
    end;
  end;
end;

function StrPosW(Start, Current: PQCharW; var ACol, ARow: Integer): PQCharW;
begin
ACol := 1;
ARow := 1;
Result := Start;
while Start < Current do
  begin
  if Start^ = #10 then
    begin
    Inc(ARow);
    ACol := 1;
    Inc(Start);
    Result := Start;
    end
  else
    begin
    Inc(Start, CharSizeW(Start));
    Inc(ACol);
    end;
  end;
end;

function DecodeTokenA(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
var
  S: PQCharA;
  l: Integer;
begin
if AIgnoreSpace then
  SkipSpaceA(p);
S := p;
while p^ <> 0 do
  begin
  if p^ = AQuoter then // 引用的内容不拆分
    begin
    Inc(p);
    while p^ <> 0 do
      begin
      if p^ = $5C then
        begin
        Inc(p);
        if p^ <> 0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInA(p, ADelimiters, @l) then
    Break
  else // \",\',"",''分别解析转义
    Inc(p);
  end;
l := IntPtr(p) - IntPtr(S);
Result.Length := l;
Move(S^, PQCharA(Result)^, l);
while CharInA(p, ADelimiters, @l) do
  Inc(p, l);
end;

function DecodeTokenU(var p: PQCharA; ADelimiters: array of QCharA;
  AQuoter: QCharA; AIgnoreSpace: Boolean): QStringA;
var
  S: PQCharA;
  l: Integer;
begin
if AIgnoreSpace then
  SkipSpaceU(p);
S := p;
while p^ <> 0 do
  begin
  if p^ = AQuoter then // 引用的内容不拆分
    begin
    Inc(p);
    while p^ <> 0 do
      begin
      if p^ = $5C then
        begin
        Inc(p);
        if p^ <> 0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInU(p, ADelimiters, @l) then
    Break
  else // \",\',"",''分别解析转义
    Inc(p);
  end;
l := IntPtr(p) - IntPtr(S);
Result.Length := l;
Move(S^, PQCharA(Result)^, l);
while CharInU(p, ADelimiters, @l) do
  Inc(p, l);
end;

function DecodeTokenW(var p: PQCharW; ADelimiters: array of QCharW;
  AQuoter: QCharW; AIgnoreSpace: Boolean): QStringW;
var
  S: PQCharW;
  l: Integer;
begin
if AIgnoreSpace then
  SkipSpaceW(p);
S := p;
while p^ <> #0 do
  begin
  if p^ = AQuoter then // 引用的内容不拆分
    begin
    Inc(p);
    while p^ <> #0 do
      begin
      if p^ = #$5C then
        begin
        Inc(p);
        if p^ <> #0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInW(p, ADelimiters, @l) then
    Break
  else // \",\',"",''分别解析转义
    Inc(p);
  end;
l := p - S;
SetLength(Result, l);
Move(S^, PQCharW(Result)^, l shl 1);
while CharInW(p, ADelimiters, @l) do
  Inc(p, l);
end;

function DecodeTokenW(var p: PQCharW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreSpace: Boolean): QStringW;
var
  S: PQCharW;
  l: Integer;
begin
if AIgnoreSpace then
  SkipSpaceW(p);
S := p;
while p^ <> #0 do
  begin
  if p^ = AQuoter then // 引用的内容不拆分
    begin
    Inc(p);
    while p^ <> #0 do
      begin
      if p^ = #$5C then
        begin
        Inc(p);
        if p^ <> #0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInW(p, ADelimiters, @l) then
    Break
  else // \",\',"",''分别解析转义
    Inc(p);
  end;
l := p - S;
SetLength(Result, l);
Move(S^, PQCharW(Result)^, l shl 1);
while CharInW(p, ADelimiters, @l) do
  Inc(p, l);
end;

function DecodeTokenW(var S: QStringW; ADelimiters: PQCharW; AQuoter: QCharW;
  AIgnoreCase, ARemove: Boolean): QStringW;
var
  p: PQCharW;
begin
p := PQCharW(S);
Result := DecodeTokenW(p, ADelimiters, AQuoter, AIgnoreCase);
if ARemove then
  S := StrDupX(p, Length(S) - (p - PQCharW(S)));
end;

function DecodeLineA(var p: PQCharA; ASkipEmpty: Boolean): QStringA;
var
  ps: PQCharA;
begin
ps := p;
while p^ <> 0 do
  begin
  if ((p^ = 13) and (PQCharA(IntPtr(p) + 1)^ = 10)) or (p^ = 10) then
    begin
    if ps = p then
      begin
      if ASkipEmpty then
        begin
        if p^ = 13 then
          Inc(p, 2)
        else
          Inc(p);
        ps := p;
        end
      else
        begin
        Result.Length := 0;
        Exit;
        end;
      end
    else
      begin
      Result.Length := IntPtr(p) - IntPtr(ps);
      Move(ps^, PQCharA(Result)^, IntPtr(p) - IntPtr(ps));
      if p^ = 13 then
        Inc(p, 2)
      else
        Inc(p);
      Exit;
      end;
    end
  else
    Inc(p);
  end;
if ps = p then
  Result.Length := 0
else
  begin
  Result.Length := IntPtr(p) - IntPtr(ps);
  Move(ps^, PQCharA(Result)^, IntPtr(p) - IntPtr(ps));
  end;
end;

function DecodeLineU(var p: PQCharA; ASkipEmpty: Boolean): QStringA;
begin
Result := DecodeLineA(p, ASkipEmpty);
if Result.Length > 0 then
  Result.FValue[0] := 1;
end;

function DecodeLineW(var p: PQCharW; ASkipEmpty: Boolean): QStringW;
var
  ps: PQCharW;
begin
ps := p;
while p^ <> #0 do
  begin
  if ((p[0] = #13) and (p[1] = #10)) or (p[0] = #10) then
    begin
    if ps = p then
      begin
      if ASkipEmpty then
        begin
        if p^ = #13 then
          Inc(p, 2)
        else
          Inc(p);
        ps := p;
        end
      else
        begin
        SetLength(Result, 0);
        Exit;
        end;
      end
    else
      begin
      SetLength(Result, p - ps);
      Move(ps^, PQCharW(Result)^, IntPtr(p) - IntPtr(ps));
      if p^ = #13 then
        Inc(p, 2)
      else
        Inc(p);
      Exit;
      end;
    end
  else
    Inc(p);
  end;
if ps = p then
  SetLength(Result, 0)
else
  begin
  SetLength(Result, p - ps);
  Move(ps^, PQCharW(Result)^, IntPtr(p) - IntPtr(ps));
  end;
end;

function LeftStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
var
  ps, p: PQCharW;
  l: Integer;
begin
l := Length(S);
if AMaxCount > l then
  Result := S
else if AMaxCount > 0 then
  begin
  ps := PQCharW(S);
  if ACheckExt then
    begin
    p := ps;
    while (p^ <> #0) and (AMaxCount > 0) do
      begin
      if (p^ >= #$DB00) and (p^ <= #$DBFF) then
        begin
        Inc(p);
        if (p^ >= #$DC00) and (p^ <= #$DFFF) then
          Inc(p);
        // else 无效的扩展区字符，仍然循环保留
        end
      else
        Inc(p);
      Dec(AMaxCount);
      end;
    l := p - ps;
    SetLength(Result, l);
    Move(ps^, PQCharW(Result)^, l shl 1);
    end
  else
    begin
    SetLength(Result, AMaxCount);
    Move(ps^, PQCharW(Result)^, AMaxCount shl 1);
    end;
  end
else
  SetLength(Result, 0);
end;

function RightStrW(const S: QStringW; AMaxCount: Integer; ACheckExt: Boolean)
  : QStringW;
var
  ps, p: PQCharW;
  l: Integer;
begin
l := Length(S);
if AMaxCount > l then
  Result := S
else if AMaxCount > 0 then
  begin
  ps := PQCharW(S);
  if ACheckExt then
    begin
    p := ps + l - 1;
    while (p > ps) and (AMaxCount > 0) do
      begin
      if (p^ >= #$DC00) and (p^ <= #$DFFF) then
        begin
        Dec(p);
        if (p^ >= #$DB00) and (p^ <= #$DBFF) then
          Dec(p)
          // else 无效的扩展区字符，仍然循环保留
        end
      else
        Dec(p);
      Dec(AMaxCount);
      end;
    Inc(p);
    l := l - (p - ps);
    SetLength(Result, l);
    Move(p^, PQCharW(Result)^, l shl 1);
    end
  else
    begin
    Inc(ps, l - AMaxCount);
    SetLength(Result, AMaxCount);
    Move(ps^, PQCharW(Result)^, AMaxCount shl 1);
    end;
  end
else
  SetLength(Result, 0);
end;

function StrBetween(var S: PQCharW; AStartTag, AEndTag: QStringW;
  AIgnoreCase: Boolean): QStringW;
var
  ps, pe: PQCharW;
  l: Integer;
begin
if AIgnoreCase then
  begin
  ps := StrIStrW(S, PQCharW(AStartTag));
  if ps <> nil then
    begin
    Inc(ps, Length(AStartTag));
    pe := StrIStrW(ps, PQCharW(AEndTag));
    if pe <> nil then
      begin
      l := pe - ps;
      SetLength(Result, l);
      Move(ps^, PQCharW(Result)^, l shl 1);
      Inc(pe, Length(AEndTag));
      S := pe;
      end
    else
      SetLength(Result, 0);
    end
  else
    SetLength(Result, 0);
  end
else
  begin
  ps := StrStrW(S, PQCharW(AStartTag));
  if ps <> nil then
    begin
    Inc(ps, Length(AStartTag));
    pe := StrStrW(ps, PQCharW(AEndTag));
    if pe <> nil then
      begin
      l := pe - ps;
      SetLength(Result, l);
      Move(ps, PQCharW(Result)^, l shl 1);
      Inc(pe, Length(AEndTag));
      S := pe;
      end
    else
      SetLength(Result, 0);
    end
  else
    SetLength(Result, 0);
  end;
end;

function TokenWithIndex(var S: PQCharW; AIndex: Integer; ADelimiters: PQCharW;
  AQuoter: QCharW; AIgnoreSapce: Boolean): QStringW;
begin
while (AIndex >= 0) and (S^ <> #0) do
  begin
  if AIndex <> 0 then
    DecodeTokenW(S, ADelimiters, AQuoter, AIgnoreSapce)
  else
    begin
    Result := DecodeTokenW(S, ADelimiters, AQuoter, AIgnoreSapce);
    Break;
    end;
  Dec(AIndex);
  end;
end;

function SkipUntilA(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA): Integer;
var
  ps: PQCharA;
begin
ps := p;
while p^ <> 0 do
  begin
  if (p^ = AQuoter) then
    begin
    Inc(p);
    while p^ <> 0 do
      begin
      if p^ = $5C then
        begin
        Inc(p);
        if p^ <> 0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInA(p, AExpects) then
    Break
  else
    Inc(p, CharSizeA(p));
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilU(var p: PQCharA; AExpects: array of QCharA;
  AQuoter: QCharA): Integer;
var
  ps: PQCharA;
begin
ps := p;
while p^ <> 0 do
  begin
  if (p^ = AQuoter) then
    begin
    Inc(p);
    while p^ <> 0 do
      begin
      if p^ = $5C then
        begin
        Inc(p);
        if p^ <> 0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInU(p, AExpects) then
    Break
  else
    Inc(p, CharSizeU(p));
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilW(var p: PQCharW; AExpects: array of QCharW;
  AQuoter: QCharW): Integer;
var
  ps: PQCharW;
begin
ps := p;
while p^ <> #0 do
  begin
  if (p^ = AQuoter) then
    begin
    Inc(p);
    while p^ <> #0 do
      begin
      if p^ = #$5C then
        begin
        Inc(p);
        if p^ <> #0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInW(p, AExpects) then
    Break
  else
    Inc(p, CharSizeW(p));
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function SkipUntilW(var p: PQCharW; AExpects: PQCharW; AQuoter: QCharW)
  : Integer;
var
  ps: PQCharW;
begin
ps := p;
while p^ <> #0 do
  begin
  if (p^ = AQuoter) then
    begin
    Inc(p);
    while p^ <> #0 do
      begin
      if p^ = #$5C then
        begin
        Inc(p);
        if p^ <> #0 then
          Inc(p);
        end
      else if p^ = AQuoter then
        begin
        Inc(p);
        if p^ = AQuoter then
          Inc(p)
        else
          Break;
        end
      else
        Inc(p);
      end;
    end
  else if CharInW(p, AExpects) then
    Break
  else
    Inc(p, CharSizeW(p));
  end;
Result := IntPtr(p) - IntPtr(ps);
end;

function CharUpperA(c: QCharA): QCharA;
begin
if (c >= $61) and (c <= $7A) then
  Result := c - $20
else
  Result := c;
end;

function CharUpperW(c: QCharW): QCharW;
begin
if (c >= #$61) and (c <= #$7A) then
  Result := QCharW(PWord(@c)^ - $20)
else
  Result := c;
end;

function StartWithA(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
begin
while (S^ <> 0) and (startby^ <> 0) do
  begin
  if AIgnoreCase then
    begin
    if CharUpperA(S^) <> CharUpperA(startby^) then
      Break;
    end
  else if S^ <> startby^ then
    Break;
  Inc(S);
  Inc(startby);
  end;
Result := (startby^ = 0);
end;

function StartWithU(S, startby: PQCharA; AIgnoreCase: Boolean): Boolean;
begin
Result := StartWithA(S, startby, AIgnoreCase);
end;

function StartWithW(S, startby: PQCharW; AIgnoreCase: Boolean): Boolean;
begin
while (S^ <> #0) and (startby^ <> #0) do
  begin
  if AIgnoreCase then
    begin
    if CharUpperW(S^) <> CharUpperW(startby^) then
      Break;
    end
  else if S^ <> startby^ then
    Break;
  Inc(S);
  Inc(startby);
  end;
Result := (startby^ = #0);
end;

function EndWithA(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
var
  p: PQCharA;
begin
if S.Length < endby.Length then
  Result := False
else
  begin
  p := PQCharA(S);
  Inc(p, S.Length - endby.Length);
  if AIgnoreCase then
    Result := (StrIStrA(p, PQCharA(endby)) = p)
  else
    Result := (StrStrA(p, PQCharA(endby)) = p);
  end;
end;

function EndWithU(const S, endby: QStringA; AIgnoreCase: Boolean): Boolean;
begin
Result := EndWithA(S, endby, AIgnoreCase);
end;

function EndWithW(const S, endby: QStringW; AIgnoreCase: Boolean): Boolean;
var
  p: PQCharW;
begin
if System.Length(S) < System.Length(endby) then
  Result := False
else
  begin
  p := PQCharW(S);
  Inc(p, System.Length(S) - System.Length(endby));
  if AIgnoreCase then
    Result := (StrIStrW(p, PQCharW(endby)) = p)
  else
    Result := (StrStrW(p, PQCharW(endby)) = p);
  end;
end;

function SameCharsA(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
begin
Result := 0;
if (s1 <> nil) and (s2 <> nil) then
  begin
  if AIgnoreCase then
    begin
    while (s1^ <> 0) and (s2^ <> 0) and
      ((s1^ = s2^) or (CharUpperA(s1^) = CharUpperA(s2^))) do
      Inc(Result);
    end
  else
    begin
    while (s1^ <> 0) and (s2^ <> 0) and (s1^ = s2^) do
      Inc(Result);
    end;
  end;
end;

function SameCharsU(s1, s2: PQCharA; AIgnoreCase: Boolean): Integer;
  function CompareSubSeq: Boolean;
  var
    ACharSize1, ACharSize2: Integer;
  begin
  ACharSize1 := CharSizeU(s1) - 1;
  ACharSize2 := CharSizeU(s2) - 1;
  Result := ACharSize1 = ACharSize2;
  if Result then
    begin
    Inc(s1);
    Inc(s2);
    while (ACharSize1 > 0) and (s1^ = s2^) do
      begin
      Inc(s1);
      Inc(s2);
      end;
    Result := (ACharSize1 = 0);
    end;
  end;

begin
Result := 0;
if (s1 <> nil) and (s2 <> nil) then
  begin
  if AIgnoreCase then
    begin
    while (s1^ <> 0) and (s2^ <> 0) and
      ((s1^ = s2^) or (CharUpperA(s1^) = CharUpperA(s2^))) do
      begin
      if CompareSubSeq then
        Inc(Result)
      else
        Break;
      end;
    end
  else
    begin
    while (s1^ <> 0) and (s2^ <> 0) and (s1^ = s2^) do
      begin
      if CompareSubSeq then
        Inc(Result)
      else
        Break;
      end;
    end;
  end;
end;

function SameCharsW(s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
begin
Result := 0;
if (s1 <> nil) and (s2 <> nil) then
  begin
  if AIgnoreCase then
    begin
    while (s1^ <> #0) and (s2^ <> #0) and
      ((s1^ = s2^) or (CharUpperW(s1^) = CharUpperW(s2^))) do
      Inc(Result);
    end
  else
    begin
    while (s1^ <> #0) and (s2^ <> #0) and (s1^ = s2^) do
      Inc(Result);
    end;
  end;
end;

function DetectTextEncoding(const p: Pointer; l: Integer; var b: Boolean)
  : TTextEncoding;
var
  pAnsi: PByte;
  pWide: PWideChar;
  I, AUtf8CharSize: Integer;
  function IsUtf8Order(var ACharSize: Integer): Boolean;
  var
    I: Integer;
    ps: PByte;
  const
    Utf8Masks: array [0 .. 4] of Byte = ($C0, $E0, $F0, $F8, $FC);
  begin
  ps := pAnsi;
  ACharSize := CharSizeU(PQCharA(ps));
  Result := False;
  if ACharSize > 1 then
    begin
    I := ACharSize - 2;
    if ((Utf8Masks[I] and ps^) = Utf8Masks[I]) then
      begin
      Inc(ps);
      Result := True;
      for I := 1 to ACharSize - 1 do
        begin
        if (ps^ and $80) <> $80 then
          begin
          Result := False;
          Break;
          end;
        Inc(ps);
        end;
      end;
    end;
  end;

begin
Result := teAnsi;
b := False;
if l >= 2 then
  begin
  pAnsi := PByte(p);
  pWide := PWideChar(p);
  b := True;
  if pWide^ = #$FEFF then
    Result := teUnicode16LE
  else if pWide^ = #$FFFE then
    Result := teUnicode16BE
  else if l >= 3 then
    begin
    if (pAnsi^ = $EF) and (PByte(IntPtr(pAnsi) + 1)^ = $BB) and
      (PByte(IntPtr(pAnsi) + 2)^ = $BF) then // UTF-8编码
      Result := teUTF8
    else // 检测字符中是否有符合UFT-8编码规则的字符，11...
      begin
      b := False;
      Result := teUTF8; // 假设文件为UTF8编码，然后检测是否有不符合UTF-8编码的序列
      I := 0;
      Dec(l, 2);
      while I <= l do
        begin
        if (pAnsi^ and $80) <> 0 then // 高位为1
          begin
          if IsUtf8Order(AUtf8CharSize) then
            begin
            if AUtf8CharSize > 2 then // 出现大于2个字节长度的UTF8序列，99%就是UTF-8了，不再判断
              Break;
            Inc(pAnsi, AUtf8CharSize);
            Inc(I, AUtf8CharSize);
            end
          else
            begin
            Result := teAnsi;
            Break;
            end;
          end
        else
          begin
          if pAnsi^ = 0 then // 00 xx (xx<128) 高位在前，是BE编码
            begin
            if PByte(IntPtr(pAnsi) + 1)^ < 128 then
              begin
              Result := teUnicode16BE;
              Break;
              end;
            end
          else if PByte(IntPtr(pAnsi) + 1)^ = 0 then // xx 00 低位在前，是LE编码
            begin
            Result := teUnicode16LE;
            Break;
            end;
          Inc(pAnsi);
          Inc(I);
          end;
        end;
      end;
    end;
  end;
end;

function LoadTextA(AFileName: String; AEncoding: TTextEncoding): QStringA;
var
  AStream: TStream;
begin
AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
try
  Result := LoadTextA(AStream, AEncoding);
finally
  AStream.Free;
end;
end;

procedure ExchangeByteOrder(p: PQCharA; l: Integer);
var
  pe: PQCharA;
  c: QCharA;
begin
pe := p;
Inc(pe, l);
while IntPtr(p) < IntPtr(pe) do
  begin
  c := p^;
  p^ := PQCharA(IntPtr(p) + 1)^;
  PQCharA(IntPtr(p) + 1)^ := c;
  Inc(p, 2);
  end;
end;

function ExchangeByteOrder(V: Smallint): Smallint;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
pd[0] := pv[1];
pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Word): Word;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
pd[0] := pv[1];
pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Integer): Integer;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
pd[0] := pv[3];
pd[1] := pv[2];
pd[2] := pv[1];
pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Cardinal): Cardinal;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
pd[0] := pv[3];
pd[1] := pv[2];
pd[2] := pv[1];
pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Int64): Int64;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
pd[0] := pv[7];
pd[1] := pv[6];
pd[2] := pv[5];
pd[3] := pv[4];
pd[4] := pv[3];
pd[5] := pv[2];
pd[6] := pv[1];
pd[7] := pv[0];
end;

function ExchangeByteOrder(V: Single): Single;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
pd[0] := pv[3];
pd[1] := pv[2];
pd[2] := pv[1];
pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Double): Double;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
pd[0] := pv[7];
pd[1] := pv[6];
pd[2] := pv[5];
pd[3] := pv[4];
pd[4] := pv[3];
pd[5] := pv[2];
pd[6] := pv[1];
pd[7] := pv[0];
end;

function LoadTextA(AStream: TStream; AEncoding: TTextEncoding): QStringA;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
ASize := AStream.Size - AStream.Position;
if ASize > 0 then
  begin
  SetLength(ABuffer, ASize);
  AStream.ReadBuffer((@ABuffer[0])^, ASize);
  if AEncoding in [teUnknown, teAuto] then
    AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
  else if ASize >= 2 then
    begin
    case AEncoding of
      teUnicode16LE:
        ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
      teUnicode16BE:
        ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
      teUTF8:
        begin
        if ASize > 3 then
          ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
            (ABuffer[2] = $BF)
        else
          ABomExists := False;
        end;
    end;
    end
  else
    ABomExists := False;
  if AEncoding = teAnsi then
    Result := ABuffer
  else if AEncoding = teUTF8 then
    begin
    if ABomExists then
      Result := AnsiEncode(Utf8Decode(@ABuffer[3], ASize - 3))
    else
      Result := AnsiEncode(Utf8Decode(@ABuffer[0], ASize));
    end
  else
    begin
    if AEncoding = teUnicode16BE then
      ExchangeByteOrder(@ABuffer[0], ASize);
    if ABomExists then
      Result := AnsiEncode(PQCharW(@ABuffer[2]), (ASize - 2) shr 1)
    else
      Result := AnsiEncode(PQCharW(@ABuffer[0]), ASize shr 1);
    end;
  end
else
  Result.Length := 0;
end;

function LoadTextU(AFileName: String; AEncoding: TTextEncoding): QStringA;
var
  AStream: TStream;
begin
AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
try
  Result := LoadTextU(AStream, AEncoding);
finally
  AStream.Free;
end;
end;

function LoadTextU(AStream: TStream; AEncoding: TTextEncoding): QStringA;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
ASize := AStream.Size - AStream.Position;
if ASize > 0 then
  begin
  SetLength(ABuffer, ASize);
  AStream.ReadBuffer((@ABuffer[0])^, ASize);
  if AEncoding in [teUnknown, teAuto] then
    AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
  else if ASize >= 2 then
    begin
    case AEncoding of
      teUnicode16LE:
        ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
      teUnicode16BE:
        ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
      teUTF8:
        begin
        if ASize > 3 then
          ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
            (ABuffer[2] = $BF)
        else
          ABomExists := False;
        end;
    end;
    end
  else
    ABomExists := False;
  if AEncoding = teAnsi then
    Result := qstring.Utf8Encode(AnsiDecode(@ABuffer[0], ASize))
  else if AEncoding = teUTF8 then
    begin
    if ABomExists then
      begin
      Dec(ASize, 3);
      Result.From(@ABuffer[0], 3, ASize);
      end
    else
      Result := ABuffer;
    if ASize > 0 then
      Result.FValue[0] := 1; // UTF-8
    end
  else
    begin
    if AEncoding = teUnicode16BE then
      ExchangeByteOrder(@ABuffer[0], ASize);
    if ABomExists then
      Result := qstring.Utf8Encode(PQCharW(@ABuffer[2]), (ASize - 2) shr 1)
    else
      Result := qstring.Utf8Encode(PQCharW(@ABuffer[0]), ASize shr 1);
    end;
  end
else
  begin
  Result.Length := 0;
  Result.FValue[0] := 1;
  end;
end;

function LoadTextW(AFileName: String; AEncoding: TTextEncoding): QStringW;
var
  AStream: TStream;
begin
AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
try
  Result := LoadTextW(AStream, AEncoding);
finally
  AStream.Free;
end;
end;

function LoadTextW(AStream: TStream; AEncoding: TTextEncoding)
  : QStringW; overload;
var
  ASize: Integer;
  ABuffer: TBytes;
  ABomExists: Boolean;
begin
ASize := AStream.Size - AStream.Position;
if ASize > 0 then
  begin
  SetLength(ABuffer, ASize);
  AStream.ReadBuffer((@ABuffer[0])^, ASize);
  if AEncoding in [teUnknown, teAuto] then
    AEncoding := DetectTextEncoding(@ABuffer[0], ASize, ABomExists)
  else if ASize >= 2 then
    begin
    case AEncoding of
      teUnicode16LE:
        ABomExists := (ABuffer[0] = $FF) and (ABuffer[1] = $FE);
      teUnicode16BE:
        ABomExists := (ABuffer[1] = $FE) and (ABuffer[1] = $FF);
      teUTF8:
        begin
        if ASize > 3 then
          ABomExists := (ABuffer[0] = $EF) and (ABuffer[1] = $BB) and
            (ABuffer[2] = $BF)
        else
          ABomExists := False;
        end;
    end;
    end
  else
    ABomExists := False;
  if AEncoding = teAnsi then
    Result := AnsiDecode(@ABuffer[0], ASize)
  else if AEncoding = teUTF8 then
    begin
    if ABomExists then
      Result := Utf8Decode(@ABuffer[3], ASize - 3)
    else
      Result := Utf8Decode(@ABuffer[0], ASize);
    end
  else
    begin
    if AEncoding = teUnicode16BE then
      ExchangeByteOrder(@ABuffer[0], ASize);
    if ABomExists then
      begin
      Dec(ASize, 2);
      SetLength(Result, ASize shr 1);
      Move(ABuffer[2], PWideChar(Result)^, ASize);
      end
    else
      begin
      SetLength(Result, ASize shr 1);
      Move(ABuffer[0], PWideChar(Result)^, ASize);
      end;
    end;
  end
else
  SetLength(Result, 0);
end;

procedure SaveTextA(AFileName: String; const S: QStringA);
var
  AStream: TFileStream;
begin
AStream := TFileStream.Create(AFileName, fmCreate);
try
  SaveTextA(AStream, S);
finally
  AStream.Free;
end;
end;

procedure SaveTextA(AStream: TStream; const S: QStringA);
  procedure Utf8Save;
  var
    T: QStringA;
  begin
  T := AnsiEncode(Utf8Decode(S));
  AStream.WriteBuffer(PQCharA(T)^, T.Length);
  end;

begin
if not S.IsUtf8 then
  AStream.WriteBuffer(PQCharA(S)^, S.Length)
else
  Utf8Save;
end;

procedure SaveTextU(AFileName: String; const S: QStringA; AWriteBom: Boolean);
var
  AStream: TFileStream;
begin
AStream := TFileStream.Create(AFileName, fmCreate);
try
  SaveTextU(AStream, S, AWriteBom);
finally
  AStream.Free;
end;
end;

procedure SaveTextU(AStream: TStream; const S: QStringA; AWriteBom: Boolean);
  procedure WriteBom;
  var
    ABom: TBytes;
  begin
  SetLength(ABom, 3);
  ABom[0] := $EF;
  ABom[1] := $BB;
  ABom[2] := $BF;
  AStream.WriteBuffer(ABom[0], 3);
  end;
  procedure SaveAnsi;
  var
    T: QStringA;
  begin
  T := qstring.Utf8Encode(AnsiDecode(S));
  AStream.WriteBuffer(PQCharA(T)^, T.Length);
  end;

begin
if AWriteBom then
  WriteBom;
if S.IsUtf8 then
  AStream.WriteBuffer(PQCharA(S)^, S.Length)
else
  SaveAnsi;
end;

procedure SaveTextW(AFileName: String; const S: QStringW; AWriteBom: Boolean);
var
  AStream: TFileStream;
begin
AStream := TFileStream.Create(AFileName, fmCreate);
try
  SaveTextW(AStream, S, AWriteBom);
finally
  AStream.Free;
end;
end;

procedure SaveTextW(AStream: TStream; const S: QStringW; AWriteBom: Boolean);
  procedure WriteBom;
  var
    bom: Word;
  begin
  bom := $FEFF;
  AStream.WriteBuffer(bom, 2);
  end;

begin
if AWriteBom then
  WriteBom;
AStream.WriteBuffer(PQCharW(S)^, System.Length(S) shl 1);
end;

procedure SaveTextWBE(AStream: TStream; const S: QStringW; AWriteBom: Boolean);
var
  pw, pe: PWord;
  w: Word;
  ABuilder: TQStringCatHelperW;
begin
pw := PWord(PQCharW(S));
pe := pw;
Inc(pe, Length(S));
ABuilder := TQStringCatHelperW.Create(IntPtr(pe) - IntPtr(pw));
try
  while IntPtr(pw) < IntPtr(pe) do
    begin
    w := (pw^ shr 8) or (pw^ shl 8);
    ABuilder.Cat(@w, 1);
    Inc(pw);
    end;
  if AWriteBom then
    AStream.WriteBuffer(#$FE#$FF, 2);
  AStream.WriteBuffer(ABuilder.FStart^, Length(S) shl 1);
finally
  FreeObject(ABuilder);
end;
end;

function StrStrA(s1, s2: PQCharA): PQCharA;
  function DoSearch: PQCharA;
  var
    ps1, ps2: PQCharA;
  begin
  ps1 := s1;
  ps2 := s2;
  Inc(ps1);
  Inc(ps2);
  while ps2^ <> 0 do
    begin
    if ps1^ = ps2^ then
      begin
      Inc(ps1);
      Inc(ps2);
      end
    else
      Break;
    end;
  if ps2^ = 0 then
    Result := s1
  else
    Result := nil;
  end;

begin
{$IFDEF MSWINDOWS}
if Assigned(VCStrStr) then
  begin
  Result := VCStrStr(s1, s2);
  Exit;
  end;
{$ENDIF}
Result := nil;
if (s1 <> nil) and (s2 <> nil) then
  begin
  while s1^ <> 0 do
    begin
    if s1^ = s2^ then
      begin
      Result := DoSearch;
      if Result <> nil then
        Exit;
      end;
    Inc(s1);
    end;
  end;
end;

function StrIStrA(s1, s2: PQCharA): PQCharA;
  function DoSearch: PQCharA;
  var
    ps1, ps2: PQCharA;
  begin
  ps1 := s1;
  ps2 := s2;
  Inc(ps1);
  Inc(ps2);
  while ps2^ <> 0 do
    begin
    if CharUpperA(ps1^) = ps2^ then
      begin
      Inc(ps1);
      Inc(ps2);
      end
    else
      Break;
    end;
  if ps2^ = 0 then
    Result := s1
  else
    Result := nil;
  end;

begin
Result := nil;
if (s1 <> nil) and (s2 <> nil) then
  begin
  while s1^ <> 0 do
    begin
    if s1^ = s2^ then
      begin
      Result := DoSearch;
      if Result <> nil then
        Exit;
      end;
    Inc(s1);
    end;
  end;
end;

function StrStrU(s1, s2: PQCharA): PQCharA;
begin
Result := StrStrA(s1, s2);
end;

function StrIStrU(s1, s2: PQCharA): PQCharA;
begin
Result := StrIStrA(s1, s2);
end;

function StrStrW(s1, s2: PQCharW): PQCharW;
var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
if Assigned(VCStrStrW) then
  begin
  Result := VCStrStrW(s1, s2);
  Exit;
  end;
{$ENDIF}
if (s2 = nil) or (s2^ = #0) then
  Result := s1
else
  begin
  Result := nil;
  while s1^ <> #0 do
    begin
    if s1^ = s2^ then
      begin
      I := 1;
      while s2[I] <> #0 do
        begin
        if s1[I] = s2[I] then
          Inc(I)
        else
          Break;
        end;
      if s2[I] = #0 then
        begin
        Result := s1;
        Break;
        end;
      end;
    Inc(s1);
    end;
  end;
end;

function StrIStrW(s1, s2: PQCharW): PQCharW;
var
  I: Integer;
  ws2: QStringW;
begin
Result := nil;
if (s1 = nil) or (s2 = nil) then
  Exit;
ws2 := UpperCase(s2);
s2 := PWideChar(ws2);
while s1^ <> #0 do
  begin
  if CharUpperW(s1^) = s2^ then
    begin
    I := 1;
    while s2[I] <> #0 do
      begin
      if CharUpperW(s1[I]) = s2[I] then
        Inc(I)
      else
        Break;
      end;
    if s2[I] = #0 then
      begin
      Result := s1;
      Break;
      end;
    end;
  Inc(s1);
  end;
end;

function StrDupX(const S: PQCharW; ACount: Integer): QStringW;
begin
SetLength(Result, ACount);
Move(S^, PQCharW(Result)^, ACount shl 1);
end;

function StrDupW(const S: PQCharW; AOffset: Integer; const ACount: Integer)
  : QStringW;
var
  c, ACharSize: Integer;
  p, pds, pd: PQCharW;
begin
c := 0;
p := S + AOffset;
SetLength(Result, 16384);
pd := PQCharW(Result);
pds := pd;
while (p^ <> #0) and (c < ACount) do
  begin
  ACharSize := CharSizeW(p);
  AOffset := pd - pds;
  if AOffset + ACharSize = Length(Result) then
    begin
    SetLength(Result, Length(Result) shl 1);
    pds := PQCharW(Result);
    pd := pds + AOffset;
    end;
  Inc(c);
  pd^ := p^;
  if ACharSize = 2 then
    pd[1] := p[1];
  Inc(pd, ACharSize);
  Inc(p, ACharSize);
  end;
SetLength(Result, pd - pds);
end;

function StrCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean): Integer;
var
  p1, p2: PQCharW;
  c1, c2: QCharW;
begin
p1 := s1;
p2 := s2;
if AIgnoreCase then
  begin
  while (p1^ <> #0) and (p2^ <> #0) do
    begin
    if p1^ <> p2^ then
      begin
      if (p1^ >= 'a') and (p1^ <= 'z') then
        c1 := WideChar(Word(p1^) xor $20)
      else
        c1 := p1^;
      if (p2^ >= 'a') and (p2^ <= 'z') then
        c2 := WideChar(Word(p2^) xor $20)
      else
        c2 := p2^;
      Result := Ord(c1) - Ord(c2);
      if Result <> 0 then
        Exit;
      end;
    Inc(p1);
    Inc(p2);
    end;
  Result := Ord(p1^) - Ord(p2^);
  end
else
  begin
  while (p1^ <> #0) and (p2^ <> #0) do
    begin
    if p1^ <> p2^ then
      begin
      Result := Ord(p1^) - Ord(p2^);
      if Result <> 0 then
        Exit;
      end;
    Inc(p1);
    Inc(p2);
    end;
  Result := Ord(p1^) - Ord(p2^);
  end;
end;

function StrNCmpW(const s1, s2: PQCharW; AIgnoreCase: Boolean;
  ALength: Integer): Integer;
var
  p1, p2: PQCharW;
  c1, c2: QCharW;
begin
p1 := s1;
p2 := s2;
if AIgnoreCase then
  begin
  while ALength > 0 do
    begin
    if p1^ <> p2^ then
      begin
      if (p1^ >= 'a') and (p1^ <= 'z') then
        c1 := WideChar(Word(p1^) xor $20)
      else
        c1 := p1^;
      if (p2^ >= 'a') and (p2^ <= 'z') then
        c2 := WideChar(Word(p2^) xor $20)
      else
        c2 := p2^;
      Result := Ord(c1) - Ord(c2);
      if Result <> 0 then
        Exit;
      end;
    Inc(p1);
    Inc(p2);
    Dec(ALength);
    end;
  Result := Ord(p1^) - Ord(p2^);
  end
else
  begin
  while ALength > 0 do
    begin
    if p1^ <> p2^ then
      begin
      Result := Ord(p1^) - Ord(p2^);
      if Result <> 0 then
        Exit;
      end;
    Inc(p1);
    Inc(p2);
    Dec(ALength);
    end;
  Result := Ord(p1^) - Ord(p2^);
  end;
end;

function IsHexChar(c: QCharW): Boolean; inline;
begin
Result := ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or
  ((c >= 'A') and (c <= 'F'));
end;

function HexValue(c: QCharW): Integer;
begin
if (c >= '0') and (c <= '9') then
  Result := Ord(c) - Ord('0')
else if (c >= 'a') and (c <= 'f') then
  Result := 10 + Ord(c) - Ord('a')
else
  Result := 10 + Ord(c) - Ord('A');
end;

function HexChar(V: Byte): QCharW;
begin
if V < 10 then
  Result := QCharW(V + Ord('0'))
else
  Result := QCharW(V - 10 + Ord('A'));
end;

function TryStrToGuid(const S: QStringW; var AGuid: TGuid): Boolean;
var
  p, ps: PQCharW;
  l: Int64;
begin
l := Length(S);
p := PWideChar(S);
if (l = 38) or (l = 36) then
  begin
  // {0BCBAAFF-15E6-451D-A8E8-0D98AC48C364}
  ps := p;
  if p^ = '{' then
    Inc(p);
  if (ParseHex(p, l) <> 8) or (p^ <> '-') then
    begin
    Result := False;
    Exit;
    end;
  AGuid.D1 := l;
  Inc(p);
  if (ParseHex(p, l) <> 4) or (p^ <> '-') then
    begin
    Result := False;
    Exit;
    end;
  AGuid.D2 := l;
  Inc(p);
  if (ParseHex(p, l) <> 4) or (p^ <> '-') then
    begin
    Result := False;
    Exit;
    end;
  AGuid.D3 := l;
  Inc(p);
  // 0102-030405060708
  // 剩下的16个字符
  l := 0;
  while IsHexChar(p[0]) do
    begin
    if IsHexChar(p[1]) then
      begin
      AGuid.D4[l] := (HexValue(p[0]) shl 4) + HexValue(p[1]);
      Inc(l);
      Inc(p, 2);
      end
    else
      begin
      Result := False;
      Exit;
      end;
    end;
  if (l <> 2) or (p^ <> '-') then
    begin
    Result := False;
    Exit;
    end;
  Inc(p);
  while IsHexChar(p[0]) do
    begin
    if IsHexChar(p[1]) then
      begin
      AGuid.D4[l] := (HexValue(p[0]) shl 4) + HexValue(p[1]);
      Inc(l);
      Inc(p, 2);
      end
    else
      begin
      Result := False;
      Exit;
      end;
    end;
  if (l = 8) then
    begin
    if ps^ = '{' then
      Result := (p[0] = '}') and (p[1] = #0)
    else
      Result := (p[0] = #0);
    end
  else
    Result := False;
  end
else
  Result := False;
end;

function StringReplaceW(const S, Old, New: QStringW; AFlags: TReplaceFlags)
  : QStringW;
var
  ps, pse, pds, pr, pd, po, pn: PQCharW;
  l, LO, LN, LS, LR: Integer;
  AReplaceOnce: Boolean;
begin
LO := Length(Old);
LN := Length(New);
LS := Length(S);
if (LO > 0) and (LS >= LO) then
  begin
  AReplaceOnce := not(rfReplaceAll in AFlags);
  // LO=LN，则不变LR=LS，假设全替换，也不过是原长度
  // LO<LN，则LR=LS+(LS*LN)/LO，假设全替换的长度
  // LO>LN，则LR=LS，假设一次都不替换，也不过是原长度
  if LO >= LN then
    LR := LS
  else if AReplaceOnce then
    LR := LS + (LN - LO)
  else
    LR := LS + 1 + LS * LN div LO;
  SetLength(Result, LR);
  ps := PQCharW(S);
  pse := ps + LS;
  pd := PQCharW(Result);
  pds := pd;
  po := PQCharW(Old);
  pn := PQCharW(New);
  repeat
    if rfIgnoreCase in AFlags then
      pr := StrIStrW(ps, po)
    else
      pr := StrStrW(ps, po);
    if pr <> nil then
      begin
      l := IntPtr(pr) - IntPtr(ps);
      Move(ps^, pd^, l);
      Inc(pd, l shr 1);
      Inc(pr, LO);
      Move(pn^, pd^, LN shl 1);
      Inc(pd, LN);
      ps := pr;
      end;
  until (pr = nil) or AReplaceOnce;
  // 将剩余部分合并到目标
  l := IntPtr(pse) - IntPtr(ps);
  Move(ps^, pd^, l);
  Inc(pd, l shr 1);
  SetLength(Result, pd - pds);
  end
else
  Result := S;
end;

function StringReplicateW(const S: QStringW; ACount: Integer): QStringW;
var
  l: Integer;
  p, ps, pd: PQCharW;
begin
l := Length(S);
if (l > 0) and (ACount > 0) then
  begin
  SetLength(Result, ACount * l);
  ps := PQCharW(S);
  pd := PQCharW(Result);
  for l := 0 to ACount - 1 do
    begin
    p := ps;
    while p^ <> #0 do
      begin
      pd^ := p^;
      Inc(pd);
      Inc(p);
      end;
    end;
  end
else
  SetLength(Result, 0);
end;

function MemScan(S: Pointer; len_s: Integer; sub: Pointer;
  len_sub: Integer): Pointer;
var
  pb_s, pb_sub, pc_sub, pc_s: PByte;
  remain: Integer;
begin
if len_s > len_sub then
  begin
  pb_s := S;
  pb_sub := sub;
  Result := nil;
  while len_s >= len_sub do
    begin
    if pb_s^ = pb_sub^ then
      begin
      remain := len_sub - 1;
      pc_sub := pb_sub;
      pc_s := pb_s;
      Inc(pc_s);
      Inc(pc_sub);
      if BinaryCmp(pc_s, pc_sub, remain) = 0 then
        begin
        Result := pb_s;
        Break;
        end;
      end;
    Inc(pb_s);
    end;
  end
else if len_s = len_sub then
  begin
  if CompareMem(S, sub, len_s) then
    Result := S
  else
    Result := nil;
  end
else
  Result := nil;
end;

function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
  function CompareByByte: Integer;
  var
    b1, b2: PByte;
  begin
  if (len <= 0) or (p1 = p2) then
    Result := 0
  else
    begin
    b1 := p1;
    b2 := p2;
    Result := 0;
    while len > 0 do
      begin
      if b1^ <> b2^ then
        begin
        Result := b1^ - b2^;
        Exit;
        end;
      Inc(b1);
      Inc(b2);
      end;
    end;
  end;

begin
{$IFDEF MSWINDOWS}
if Assigned(VCMemCmp) then
  Result := VCMemCmp(p1, p2, len)
else
  Result := CompareByByte;
{$ELSE}
Result := memcmp(p1, p2, len);
{$ENDIF}
end;

procedure SkipHex(var S: PQCharW);
begin
while ((S^ >= '0') and (S^ <= '9')) or ((S^ >= 'a') and (S^ <= 'f')) or
  ((S^ >= 'A') and (S^ <= 'F')) do
  Inc(S);
end;

procedure SkipDec(var S: PQCharW);
begin
while (S^ >= '0') and (S^ <= '9') do
  Inc(S);
end;

function ParseHex(var p: PQCharW; var Value: Int64): Integer;
var
  ps: PQCharW;
begin
Value := 0;
ps := p;
while IsHexChar(p^) do
  begin
  Value := (Value shl 4) + HexValue(p^);
  Inc(p);
  end;
Result := p - ps;
end;

function LeftStrCount(const S: QStringW; const sub: QStringW): Integer;
var
  ps, psub: PQCharW;
  l: Integer;
begin
l := Length(sub);
Result := 0;
if Length(S) > l then
  begin
  ps := PQCharW(S);
  psub := PQCharW(sub);
  while ps^ <> #0 do
    begin
    if CompareMem(ps, psub, l shl 1) then
      begin
      Inc(Result);
      Inc(ps, l);
      end
    else
      Break;
    end;
  end;
end;

function RightStrCount(const S: QStringW; const sub: QStringW): Integer;
var
  ps, pe, psub: PQCharW;
  l: Integer;
begin
l := Length(sub);
Result := 0;
if Length(S) > l then
  begin
  ps := PQCharW(S);
  pe := ps + l - 1;
  psub := PQCharW(sub);
  while pe > ps do
    begin
    if CompareMem(pe - l, psub, l shl 1) then
      begin
      Inc(Result);
      Dec(ps, l);
      end
    else
      Break;
    end;
  end;
end;

function ParseInt(var S: PQCharW; var ANum: Int64): Integer;
var
  ps: PQCharW;
  ANeg: Boolean;
begin
ps := S;
// 跳过16进制开始字符
if S[0] = '$' then
  begin
  Inc(S);
  Result := ParseHex(S, ANum);
  end
else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
  Inc(S, 2);
  Result := ParseHex(S, ANum);
  end
else
  begin
  if (S^ = '-') then
    begin
    ANeg := True;
    Inc(S);
    end
  else
    begin
    ANeg := False;
    if S^ = '+' then
      Inc(S);
    end;
  ANum := 0;
  while (S^ >= '0') and (S^ <= '9') do
    begin
    ANum := ANum * 10 + Ord(S^) - Ord('0');
    Inc(S);
    end;
  if ANeg then
    ANum := -ANum;
  Result := S - ps;
  end;
end;

function ParseNumeric(var S: PQCharW; var ANum: Extended): Boolean;
var
  ps: PQCharW;
  function ParseHexInt: Boolean;
  var
    iVal: Int64;
  begin
  iVal := 0;
  while IsHexChar(S^) do
    begin
    iVal := (iVal shl 4) + HexValue(S^);
    Inc(S);
    end;
  Result := (S <> ps);
  ANum := iVal;
  end;

  function ParseDec: Boolean;
  var
    ACount: Integer;
    iVal: Int64;
    APow: Extended;
  begin
  ParseInt(S, iVal);
  ANum := iVal;
  if S^ = '.' then // 小数部分
    begin
    Inc(S);
    ACount := ParseInt(S, iVal);
    if ACount > 0 then
      ANum := ANum + iVal / IntPower(10, ACount);
    end;
  if (S^ = 'e') or (S^ = 'E') then
    begin
    Inc(S);
    if ParseNumeric(S, APow) then
      ANum := ANum * Power(10, APow);
    end;
  Result := (S <> ps);
  end;

begin
ps := S;
if S^ = '$' then
  begin
  Inc(S);
  Result := ParseHexInt;
  Exit;
  end
else if (S[0] = '0') and ((S[1] = 'x') or (S[1] = 'X')) then
  begin
  Inc(S, 2);
  Result := ParseHexInt;
  Exit;
  end
else
  Result := ParseDec;
end;

function NameOfW(const S: QStringW; ASpliter: QCharW): QStringW;
var
  p: PQCharW;
begin
p := PQCharW(S);
Result := DecodeTokenW(p, [ASpliter], WideChar(0), False);
end;

function ValueOfW(const S: QStringW; ASpliter: QCharW): QStringW;
var
  p: PQCharW;
  l: Integer;
begin
p := PQCharW(S);
if p^ = ASpliter then
  begin
  l := Length(S);
  Dec(l);
  SetLength(Result, l);
  Inc(p);
  Move(p^, PQCharW(Result)^, l shl 1);
  end
else
  begin
  DecodeTokenW(p, [ASpliter], WideChar(0), False);
  if p^ <> #0 then
    Result := p
  else
    Result := S;
  end;
end;

function IndexOfNameW(AList: TStrings; const AName: QStringW;
  ASpliter: QCharW): Integer;
var
  I: Integer;
begin
Result := -1;
for I := 0 to AList.count - 1 do
  begin
  if NameOfW(AList[I], ASpliter) = AName then
    begin
    Result := I;
    Break;
    end;
  end;
end;

function IndexOfValueW(AList: TStrings; const AValue: QStringW;
  ASpliter: QCharW): Integer;
var
  I: Integer;
begin
Result := -1;
for I := 0 to AList.count - 1 do
  begin
  if ValueOfW(AList[I], ASpliter) = AValue then
    begin
    Result := I;
    Break;
    end;
  end;
end;

function DeleteCharW(const ASource, ADeletes: QStringW): QStringW;
var
  ps, pd: PQCharW;
  l, ACharLen: Integer;
begin
l := Length(ASource);
if (l > 0) and (Length(ADeletes) > 0) then
  begin
  SetLength(Result, l);
  ps := PQCharW(ASource);
  pd := PQCharW(Result);
  while l > 0 do
    begin
    if not CharInW(ps, PQCharW(ADeletes), @ACharLen) then
      begin
      pd^ := ps^;
      Inc(pd);
      ACharLen := CharSizeW(ps);
      end;
    Inc(ps, ACharLen);
    Dec(l, ACharLen);
    end;
  SetLength(Result, pd - PQCharW(Result));
  end
else
  Result := ASource;
end;

function ContainsCharW(const S, ACharList: QStringW): Boolean;
var
  ps: PQCharW;
  l: Integer;
begin
l := Length(S);
Result := False;
if (l > 0) then
  begin
  if Length(ACharList) > 0 then
    begin
    ps := PQCharW(S);
    while l > 0 do
      begin
      if CharInW(ps, PQCharW(ACharList)) then
        begin
        Result := True;
        Break;
        end;
      Inc(ps);
      Dec(l);
      end;
    end;
  end;
end;

procedure strcpyW(var d: PQCharW; S: PQCharW);
begin
while S^ <> #0 do
  begin
  d^ := S^;
  Inc(d);
  Inc(S);
  end;
end;

function HtmlEscape(const S: QStringW): QStringW;
var
  p, pd: PQCharW;
  AFound: Boolean;
  I: Integer;
begin
if Length(S) > 0 then
  begin
  System.SetLength(Result, Length(S) shl 3); // 转义串最长不超过8个字符，长度*8肯定够了
  p := PWideChar(S);
  pd := PWideChar(Result);
  while p^ <> #0 do
    begin
    AFound := False;
    for I := 0 to 91 do
      begin
      if HtmlEscapeChars[I shl 1] = p^ then
        begin
        AFound := True;
        strcpyW(pd, PQCharW(HtmlEscapeChars[(I shl 1) + 1]));
        Break;
        end;
      end; // end for
    if not AFound then
      begin
      pd^ := p^;
      Inc(pd);
      end; // end if
    Inc(p);
    end; // end while
  SetLength(Result, pd - PQCharW(Result));
  end // end if
else
  Result := '';
end;

function HtmlUnescape(const S: QStringW): QStringW;
var
  p, pd, ps: PQCharW;
  AFound: Boolean;
  I, l: Integer;
begin
if Length(S) > 0 then
  begin
  System.SetLength(Result, Length(S));
  p := PQCharW(S);
  pd := PQCharW(Result);
  while p^ <> #0 do
    begin
    if p^ = '&' then
      begin
      if p[1] = '#' then
        begin
        ps := p;
        Inc(p, 2);
        l := 0;
        while (p^ >= '0') and (p^ <= '9') do
          begin
          l := l * 10 + Ord(p^) - Ord('0');
          Inc(p);
          end;
        if p^ = ';' then
          begin
          pd^ := QCharW(l);
          Inc(pd);
          end
        else
          begin
          pd^ := ps^;
          Inc(pd);
          p := ps;
          end;
        end
      else
        begin
        AFound := False;
        for I := 0 to 91 do
          begin
          if StrStrW(p, PWideChar(HtmlEscapeChars[I shl 1 + 1])) = p then
            begin
            AFound := True;
            strcpyW(pd, PQCharW(HtmlEscapeChars[(I shl 1)]));
            Break;
            end;
          end; // end for
        if AFound then
          begin
          Inc(p, Length(HtmlEscapeChars[I shl 1 + 1]));
          continue;
          end
        else
          begin
          pd^ := p^;
          Inc(pd);
          end; // end if
        end; // end else
      end // end else
    else
      begin
      pd^ := p^;
      Inc(pd);
      end;
    Inc(p);
    end; // end while
  SetLength(Result, pd - PWideChar(Result));
  end // end if
else
  Result := '';
end;

function HtmlTrimText(const S: QStringW): QStringW;
var
  ps, pe: PQCharW;
  l: Integer;
begin
if Length(S) > 0 then
  begin
  ps := PQCharW(S);
  pe := ps + System.Length(S) - 1;
  while IsSpaceW(ps) do
    Inc(ps);
  while IsSpaceW(pe) do
    Dec(pe);
  l := pe - ps + 1;
  SetLength(Result, l);
  Move(ps^, PQCharW(Result)^, l shl 1);
  end
else
  Result := '';
end;

// 下面是一些辅助函数
function ParseDateTime(S: PWideChar; var AResult: TDateTime): Boolean;
var
  Y, M, d, H, N, Sec, MS: Word;
  AQuoter: WideChar;
  ADate: TDateTime;
  function ParseNum(var N: Word): Boolean;
  var
    neg: Boolean;
    ps: PQCharW;
  begin
  N := 0;
  ps := S;
  if S^ = '-' then
    begin
    neg := True;
    Inc(S);
    end
  else
    neg := False;
  while S^ <> #0 do
    begin
    if (S^ >= '0') and (S^ <= '9') then
      begin
      N := N * 10 + Ord(S^) - 48;
      Inc(S);
      end
    else
      Break;
    end;
  if neg then
    N := -N;
  Result := ps <> S;
  end;

begin
if (S^ = '"') or (S^ = '''') then
  begin
  AQuoter := S^;
  Inc(S);
  end
else
  AQuoter := #0;
Result := ParseNum(Y);
if not Result then
  Exit;
if S^ = '-' then
  begin
  Inc(S);
  Result := ParseNum(M);
  if (not Result) or (S^ <> '-') then
    Exit;
  Inc(S);
  Result := ParseNum(d);
  if (not Result) or ((S^ <> 'T') and (S^ <> ' ') and (S^ <> #0)) then
    Exit;
  if S^ <> #0 then
    Inc(S);
  Result := TryEncodeDate(Y, M, d, ADate);
  if not Result then
    Exit;
  SkipSpaceW(S);
  if S^ <> #0 then
    begin
    if not ParseNum(H) then // 没跟时间值
      begin
      AResult := ADate;
      Exit;
      end;
    if S^ <> ':' then
      begin
      if H in [0 .. 23] then
        AResult := ADate + EncodeTime(H, 0, 0, 0)
      else
        Result := False;
      Exit;
      end;
    Inc(S);
    end
  else
    begin
    AResult := ADate;
    Exit;
    end;
  end
else if S^ = ':' then
  begin
  ADate := 0;
  H := Y;
  Inc(S);
  end
else
  begin
  Result := False;
  Exit;
  end;
if H > 23 then
  begin
  Result := False;
  Exit;
  end;
if not ParseNum(N) then
  begin
  if AQuoter <> #0 then
    begin
    if S^ = AQuoter then
      AResult := ADate + EncodeTime(H, 0, 0, 0)
    else
      Result := False;
    end
  else
    AResult := ADate + EncodeTime(H, 0, 0, 0);
  Exit;
  end
else if N > 59 then
  begin
  Result := False;
  Exit;
  end;
Sec := 0;
MS := 0;
if S^ = ':' then
  begin
  Inc(S);
  if not ParseNum(Sec) then
    begin
    if AQuoter <> #0 then
      begin
      if S^ = AQuoter then
        AResult := ADate + EncodeTime(H, N, 0, 0)
      else
        Result := False;
      end
    else
      AResult := ADate + EncodeTime(H, N, 0, 0);
    Exit;
    end
  else if Sec > 59 then
    begin
    Result := False;
    Exit;
    end;
  if S^ = '.' then
    begin
    Inc(S);
    if not ParseNum(MS) then
      begin
      if AQuoter <> #0 then
        begin
        if AQuoter = S^ then
          AResult := ADate + EncodeTime(H, N, Sec, 0)
        else
          Result := False;
        end
      else
        AResult := ADate + EncodeTime(H, N, Sec, 0);
      Exit;
      end
    else if MS >= 1000 then // 超过1000是以微秒为单位计时的，转换为毫秒
      begin
      while MS >= 1000 do
        MS := MS div 10;
      end;
    if AQuoter <> #0 then
      begin
      if AQuoter = S^ then
        AResult := ADate + EncodeTime(H, N, Sec, MS)
      else
        Result := False;
      Exit;
      end
    else
      AResult := ADate + EncodeTime(H, N, Sec, MS);
    end
  else
    begin
    if AQuoter <> #0 then
      begin
      if AQuoter = S^ then
        AResult := ADate + EncodeTime(H, N, Sec, 0)
      else
        Result := False;
      end
    else
      AResult := ADate + EncodeTime(H, N, Sec, 0)
    end;
  end
else
  begin
  if AQuoter <> #0 then
    begin
    if AQuoter = S^ then
      AResult := ADate + EncodeTime(H, N, 0, 0)
    else
      Result := False;
    end
  else
    AResult := ADate + EncodeTime(H, N, 0, 0);
  end;
end;

function ParseWebTime(p: PWideChar; var AResult: TDateTime): Boolean;
var
  I: Integer;
  Y, M, d, H, N, S: Integer;
const
  MonthNames: array [0 .. 11] of QStringW = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
    'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  Comma: PWideChar = ',';
  Digits: PWideChar = '0123456789';
begin
// 跳过星期，这个可以直接通过日期计算出来，不需要
SkipUntilW(p, Comma, WideChar(0));
if p^ = #0 then
  begin
  Result := False;
  Exit;
  end
else
  Inc(p);
SkipUntilW(p, Digits, WideChar(0));
d := 0;
// 日期
while (p^ >= '0') and (p^ <= '9') do
  begin
  d := d * 10 + Ord(p^) - Ord('0');
  Inc(p);
  end;
if (d < 1) or (d > 31) then
  begin
  Result := False;
  Exit;
  end;
SkipSpaceW(p);
M := 0;
for I := 0 to 11 do
  begin
  if StartWithW(p, PWideChar(MonthNames[I]), True) then
    begin
    M := I + 1;
    Break;
    end;
  end;
if (M < 1) or (M > 12) then
  begin
  Result := False;
  Exit;
  end;
while (p^ <> #0) and ((p^ < '0') or (p^ > '9')) do
  Inc(p);
Y := 0;
while (p^ >= '0') and (p^ <= '9') do
  begin
  Y := Y * 10 + Ord(p^) - Ord('0');
  Inc(p);
  end;
while p^ = ' ' do
  Inc(p);
H := 0;
while (p^ >= '0') and (p^ <= '9') do
  begin
  H := H * 10 + Ord(p^) - Ord('0');
  Inc(p);
  end;
while p^ = ':' do
  Inc(p);
N := 0;
while (p^ >= '0') and (p^ <= '9') do
  begin
  N := N * 10 + Ord(p^) - Ord('0');
  Inc(p);
  end;
while p^ = ':' do
  Inc(p);
S := 0;
while (p^ >= '0') and (p^ <= '9') do
  begin
  S := S * 10 + Ord(p^) - Ord('0');
  Inc(p);
  end;
while p^ = ':' do
  Inc(p);
Result := TryEncodeDateTime(Y, M, d, H, N, S, 0, AResult);
end;

function RollupSize(ASize: Int64): QStringW;
var
  AIdx, R1, s1: Int64;
  AIsNeg: Boolean;
const
  Units: array [0 .. 3] of QStringW = ('GB', 'MB', 'KB', 'B');
begin
AIsNeg := (ASize < 0);
AIdx := 3;
R1 := 0;
if AIsNeg then
  ASize := -ASize;
Result := '';
while (AIdx >= 0) do
  begin
  s1 := ASize mod 1024;
  ASize := ASize shr 10;
  if (ASize = 0) or (AIdx = 0) then
    begin
    R1 := R1 * 100 div 1024;
    if R1 > 0 then
      begin
      if R1 >= 10 then
        Result := IntToStr(s1) + '.' + IntToStr(R1) + Units[AIdx]
      else
        Result := IntToStr(s1) + '.' + '0' + IntToStr(R1) + Units[AIdx];
      end
    else
      Result := IntToStr(s1) + Units[AIdx];
    Break;
    end;
  R1 := s1;
  Dec(AIdx);
  end;
if AIsNeg then
  Result := '-' + Result;
end;

function RollupTime(ASeconds: Int64): QStringW;
var
  H, N, d: Integer;
begin
Result := '';
d := ASeconds div 86400;
ASeconds := ASeconds mod 86400;
H := ASeconds div 3600;
ASeconds := ASeconds mod 3600;
N := ASeconds div 60;
ASeconds := ASeconds mod 60;
if d > 0 then
  Result := IntToStr(d) + SDayName
else
  Result := '';
if H > 0 then
  Result := Result + IntToStr(H) + SHourName;
if N > 0 then
  Result := Result + IntToStr(N) + SMinuteName;
if ASeconds > 0 then
  Result := Result + IntToStr(ASeconds) + SSecondName;
end;
{ QStringA }

procedure QStringA.From(p: PQCharA; AOffset, ALen: Integer);
begin
SetLength(ALen);
Inc(p, AOffset);
Move(p^, PQCharA(@FValue[1])^, ALen);
end;

function QStringA.GetChars(AIndex: Integer): QCharA;
begin
if (AIndex < 0) or (AIndex >= Length) then
  raise Exception.CreateFmt(SOutOfIndex, [AIndex, 0, Length - 1]);
Result := FValue[AIndex + 1];
end;

class operator QStringA.Implicit(const S: QStringW): QStringA;
begin
Result := qstring.AnsiEncode(S);
end;

class operator QStringA.Implicit(const S: QStringA): PQCharA;
begin
Result := PQCharA(@S.FValue[1]);
end;

function QStringA.GetIsUtf8: Boolean;
begin
if System.Length(FValue) > 0 then
  Result := (FValue[0] = 1)
else
  Result := False;
end;

function QStringA.GetLength: Integer;
begin
// QStringA.FValue[0]存贮编码类型，0-ANSI,1-UTF8，末尾存贮字符串的\0结束符
Result := System.Length(FValue);
if Result >= 2 then
  Dec(Result, 2)
else
  Result := 0;
end;

class operator QStringA.Implicit(const S: QStringA): TBytes;
var
  l: Integer;
begin
l := System.Length(S.FValue) - 1;
System.SetLength(Result, l);
if l > 0 then
  Move(S.FValue[1], Result[0], l);
end;

procedure QStringA.SetChars(AIndex: Integer; const Value: QCharA);
begin
if (AIndex < 0) or (AIndex >= Length) then
  raise Exception.CreateFmt(SOutOfIndex, [AIndex, 0, Length - 1]);
FValue[AIndex + 1] := Value;
end;

procedure QStringA.SetLength(const Value: Integer);
begin
if Value < 0 then
  begin
  if System.Length(FValue) > 0 then
    System.SetLength(FValue, 1)
  else
    begin
    System.SetLength(FValue, 1);
    FValue[0] := 0; // ANSI
    end;
  end
else
  begin
  System.SetLength(FValue, Value + 2);
  FValue[Value + 1] := 0;
  end;
end;

class operator QStringA.Implicit(const ABytes: TBytes): QStringA;
var
  l: Integer;
begin
l := System.Length(ABytes);
Result.Length := l;
if l > 0 then
  Move(ABytes[0], Result.FValue[1], l);
end;

class operator QStringA.Implicit(const S: QStringA): QStringW;
begin
Result := AnsiDecode(S);
end;

function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): QStringW;
const
  B2HConvert: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of QCharW = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PQCharW;
  pb: PByte;
begin
SetLength(Result, l shl 1);
pd := PQCharW(Result);
pb := p;
if ALowerCase then
  begin
  while l > 0 do
    begin
    pd^ := B2HConvertL[pb^ shr 4];
    Inc(pd);
    pd^ := B2HConvertL[pb^ and $0F];
    Inc(pd);
    Inc(pb);
    Dec(l);
    end;
  end
else
  begin
  while l > 0 do
    begin
    pd^ := B2HConvert[pb^ shr 4];
    Inc(pd);
    pd^ := B2HConvert[pb^ and $0F];
    Inc(pd);
    Inc(pb);
    Dec(l);
    end;
  end;
end;

function BinToHex(const ABytes: TBytes; ALowerCase: Boolean): QStringW;
begin
Result := BinToHex(@ABytes[0], Length(ABytes), ALowerCase);
end;

procedure HexToBin(const S: QStringW; var AResult: TBytes);
var
  l: Integer;
  p, ps: PQCharW;
  pd: PByte;
begin
l := System.Length(S);
SetLength(AResult, l shr 1);
p := PQCharW(S);
ps := p;
pd := @AResult[0];
while p - ps < l do
  begin
  if IsHexChar(p[0]) and IsHexChar(p[1]) then
    begin
    pd^ := (HexValue(p[0]) shl 4) + HexValue(p[1]);
    Inc(pd);
    Inc(p, 2);
    end
  else
    begin
    SetLength(AResult, 0);
    Exit;
    end;
  end;
end;

function HexToBin(const S: QStringW): TBytes;
begin
HexToBin(S, Result);
end;

procedure FreeObject(AObject: TObject);
begin
{$IFDEF AUTOREFCOUNT}
AObject.DisposeOf;
{$ELSE}
AObject.Free;
{$ENDIF}
end;

function HashOf(const p: Pointer; l: Integer): Cardinal;
var
  ps: PCardinal;
  LR: Integer;
begin
Result := 0;
if l > 0 then
  begin
  ps := p;
  LR := (l and $03); // 检查长度是否为4的整数倍
  l := (l and $FFFFFFFC); // 整数长度
  while l > 0 do
    begin
    Result := ((Result shl 5) or (Result shr 27)) xor ps^;
    Inc(ps);
    Dec(l, 4);
    end;
  if LR <> 0 then
    begin
    // lr肯定是1,2,3
    case LR of
      1:
        l := PByte(ps)^;
      2:
        l := PWord(ps)^;
      3:
        l := PWord(ps)^ or (PByte(IntPtr(ps) + 2)^ shl 16);
    end;
    Result := ((Result shl 5) or (Result shr 27)) xor Cardinal(l);
    end;
  end;
end;

class operator QStringA.Implicit(const S: PQCharA): QStringA;
var
  p: PQCharA;
begin
if S <> nil then
  begin
  p := S;
  while p^ <> 0 do
    Inc(p);
  Result.Length := IntPtr(p) - IntPtr(S);
  Move(S^, PQCharA(Result)^, Result.Length);
  end
else
  Result.Length := 0;
end;

{ TQStringCatHelperW }

function TQStringCatHelperW.Back(ALen: Integer): TQStringCatHelperW;
begin
Result := Self;
Dec(FDest, ALen);
if FDest < FStart then
  FDest := FStart;
end;

function TQStringCatHelperW.BackIf(const S: PQCharW): TQStringCatHelperW;
var
  ps: PQCharW;
begin
Result := Self;
ps := FStart;
while FDest > ps do
  begin
  if (FDest[-1] >= #$DC00) and (FDest[-1] <= #$DFFF) then
    begin
    if CharInW(FDest - 2, S) then
      Dec(FDest, 2)
    else
      Break;
    end
  else if CharInW(FDest - 1, S) then
    Dec(FDest)
  else
    Break;
  end;
end;

function TQStringCatHelperW.Cat(const S: QStringW): TQStringCatHelperW;
begin
Result := Cat(PQCharW(S), Length(S));
end;

function TQStringCatHelperW.Cat(p: PQCharW; len: Integer): TQStringCatHelperW;
begin
Result := Self;
if len < 0 then
  begin
  while p^ <> #0 do
    begin
    if IntPtr(FDest) - IntPtr(FStart) >= FSize then
      NeedSize(FSize + FBlockSize);
    FDest^ := p^;
    Inc(p);
    Inc(FDest);
    end;
  end
else
  begin
  NeedSize(-len);
  Move(p^, FDest^, len shl 1);
  Inc(FDest, len);
  end;
end;

function TQStringCatHelperW.Cat(c: QCharW): TQStringCatHelperW;
begin
if (IntPtr(FDest) - IntPtr(FStart)) = FSize then
  NeedSize(-1);
FDest^ := c;
Inc(FDest);
Result := Self;
end;

function TQStringCatHelperW.Cat(const V: Double): TQStringCatHelperW;
begin
Result := Cat(FloatToStr(V));
end;

function TQStringCatHelperW.Cat(const V: Int64): TQStringCatHelperW;
begin
Result := Cat(IntToStr(V));
end;

function TQStringCatHelperW.Cat(const V: Boolean): TQStringCatHelperW;
begin
Result := Cat(BoolToStr(V, True));
end;

function TQStringCatHelperW.Cat(const V: TGuid): TQStringCatHelperW;
begin
Result := Cat(GuidToString(V));
end;

function TQStringCatHelperW.Cat(const V: Currency): TQStringCatHelperW;
begin
Result := Cat(CurrToStr(V));
end;

constructor TQStringCatHelperW.Create(ASize: Integer);
begin
inherited Create;
FBlockSize := ASize;
NeedSize(FBlockSize);
end;

constructor TQStringCatHelperW.Create;
begin
inherited Create;
FBlockSize := 8192;
NeedSize(FBlockSize);
end;

function TQStringCatHelperW.GetChars(AIndex: Integer): QCharW;
begin
Result := FStart[AIndex];
end;

function TQStringCatHelperW.GetPosition: Integer;
begin
Result := FDest - FStart;
end;

function TQStringCatHelperW.GetValue: QStringW;
var
  l: Integer;
begin
l := Position;
SetLength(Result, l);
Move(FStart^, PQCharW(Result)^, l shl 1);
end;

procedure TQStringCatHelperW.NeedSize(ASize: Integer);
var
  offset: Integer;
begin
offset := FDest - FStart;
if ASize < 0 then
  ASize := offset - ASize;
if ASize > FSize then
  begin
  FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
  SetLength(FValue, FSize);
  FStart := PQCharW(@FValue[0]);
  FDest := FStart + offset;
  end;
end;

function TQStringCatHelperW.Replicate(const S: QStringW; count: Integer)
  : TQStringCatHelperW;
var
  ps: PQCharW;
  l: Integer;
begin
Result := Self;
if count > 0 then
  begin
  ps := PQCharW(S);
  l := Length(S);
  while count > 0 do
    begin
    Cat(ps, l);
    Dec(count);
    end;
  end;
end;

procedure TQStringCatHelperW.Reset;
begin
FDest := FStart;
end;

procedure TQStringCatHelperW.SetPosition(const Value: Integer);
begin
if Value <= 0 then
  FDest := FStart
else if Value > Length(FValue) then
  begin
  NeedSize(Value);
  FDest := FStart + Value;
  end
else
  FDest := FStart + Value;
end;

function TQStringCatHelperW.Cat(const V: Variant): TQStringCatHelperW;
begin
Result := Cat(VarToStr(V));
end;

{ TQPtr }

class function TQPtr.Bind(AObject: TObject): IQPtr;
begin
Result := TQPtr.Create(AObject);
end;

constructor TQPtr.Create(AObject: TObject);
begin
inherited Create;
FObject := AObject;
end;

destructor TQPtr.Destroy;
begin
FreeAndNil(FObject);
inherited;
end;

function TQPtr.Get: Pointer;
begin
Result := FObject;
end;

// 兼容2007版的原子操作接口
{$IF RTLVersion<26}

function AtomicCmpExchange(var Target: Integer; Value: Integer;
  Comparand: Integer): Integer; inline;
begin
Result := InterlockedCompareExchange(Target, Value, Comparand);
end;

function AtomicIncrement(var Target: Integer): Integer; inline;
begin
Result := InterlockedIncrement(Target);
end;

function AtomicDecrement(var Target: Integer): Integer; inline;
begin
Result := InterlockedDecrement(Target);
end;

function AtomicExchange(var Target: Integer; Value: Integer): Integer;
begin
Result := InterlockedExchange(Target, Value);
end;
{$IFEND <XE5}

// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  I: Integer;
begin
repeat
  Result := Dest;
  I := Result and AMask;
until AtomicCmpExchange(Dest, I, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  I: Integer;
begin
repeat
  Result := Dest;
  I := Result or AMask;
until AtomicCmpExchange(Dest, I, Result) = Result;
end;

{ TQBytesCatHelper }

function TQBytesCatHelper.Back(ALen: Integer): TQBytesCatHelper;
begin
Result := Self;
Dec(FDest, ALen);
if IntPtr(FDest) < IntPtr(FStart) then
  FDest := FStart;
end;

function TQBytesCatHelper.Cat(const V: Double): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Double));
end;

function TQBytesCatHelper.Cat(const V: Currency): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Currency));
end;

function TQBytesCatHelper.Cat(const V: Boolean): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Boolean));
end;

function TQBytesCatHelper.Cat(const S: QStringW): TQBytesCatHelper;
begin
Result := Cat(PQCharW(S), System.Length(S) shl 1);
end;

function TQBytesCatHelper.Cat(const V: Byte): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Byte));
end;

function TQBytesCatHelper.Cat(const V: Int64): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Int64));
end;

function TQBytesCatHelper.Cat(const c: QCharW): TQBytesCatHelper;
begin
Result := Cat(@c, SizeOf(QCharW));
end;

function TQBytesCatHelper.Cat(const V: Variant): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Variant));
end;

function TQBytesCatHelper.Cat(const V: QStringA; ACStyle: Boolean)
  : TQBytesCatHelper;
begin
if ACStyle then
  Result := Cat(PQCharA(V), V.Length + 1)
else
  Result := Cat(PQCharA(V), V.Length);
end;

{$IFNDEF NEXTGEN}

function TQBytesCatHelper.Cat(const V: AnsiChar): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(AnsiChar));
end;

function TQBytesCatHelper.Cat(const V: AnsiString): TQBytesCatHelper;
begin
Result := Cat(PAnsiChar(V), System.Length(V));
end;
{$ENDIF}

function TQBytesCatHelper.Cat(const V: Single): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Single));
end;

function TQBytesCatHelper.Cat(const V: Cardinal): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Cardinal));
end;

function TQBytesCatHelper.Cat(const V: Smallint): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Smallint));
end;

function TQBytesCatHelper.Cat(const V: Word): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Word));
end;

function TQBytesCatHelper.Cat(const V: Shortint): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Shortint));
end;

function TQBytesCatHelper.Cat(const V: Integer): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(Integer));
end;

function TQBytesCatHelper.Cat(const ABytes: TBytes): TQBytesCatHelper;
begin
if Length(ABytes) > 0 then
  Result := Cat(@ABytes[0], Length(ABytes))
else
  Result := Self;
end;

function TQBytesCatHelper.Cat(const AData: Pointer; const ALen: Integer)
  : TQBytesCatHelper;
begin
Result := Self;
NeedSize(-ALen);
Move(AData^, FDest^, ALen);
Inc(FDest, ALen);
end;

function TQBytesCatHelper.Cat(const V: TGuid): TQBytesCatHelper;
begin
Result := Cat(@V, SizeOf(TGuid));
end;

constructor TQBytesCatHelper.Create(ASize: Integer);
begin
inherited Create;
FBlockSize := ASize;
NeedSize(FBlockSize);
end;

constructor TQBytesCatHelper.Create;
begin
inherited Create;
FBlockSize := 8192;
NeedSize(FBlockSize);
end;

function TQBytesCatHelper.GetBytes(AIndex: Integer): Byte;
begin
Result := FValue[AIndex];
end;

function TQBytesCatHelper.GetPosition: Integer;
begin
Result := IntPtr(FDest) - IntPtr(FStart);
end;

procedure TQBytesCatHelper.NeedSize(ASize: Integer);
var
  offset: Integer;
begin
offset := IntPtr(FDest) - IntPtr(FStart);
if ASize < 0 then
  ASize := offset - ASize;
if ASize > FSize then
  begin
  FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
  SetLength(FValue, FSize);
  FStart := @FValue[0];
  FDest := PByte(IntPtr(FStart) + offset);
  end;
end;

function TQBytesCatHelper.Replicate(const ABytes: TBytes; ACount: Integer)
  : TQBytesCatHelper;
var
  l: Integer;
begin
Result := Self;
l := Length(ABytes);
if l > 0 then
  begin
  NeedSize(-l * ACount);
  while ACount > 0 do
    begin
    Move(ABytes[0], FDest^, l);
    Inc(FDest, l);
    Dec(ACount);
    end;
  end;
end;

procedure TQBytesCatHelper.Reset;
begin
FDest := FStart;
end;

procedure TQBytesCatHelper.SetCapacity(const Value: Integer);
begin
if FSize <> Value then
  NeedSize(Value);
end;

procedure TQBytesCatHelper.SetPosition(const Value: Integer);
begin
if Value <= 0 then
  FDest := FStart
else if Value > Length(FValue) then
  begin
  NeedSize(Value);
  FDest := Pointer(IntPtr(FStart) + Value);
  end
else
  FDest := Pointer(IntPtr(FStart) + Value);
end;

initialization

{$IFDEF MSWINDOWS}
  hMsvcrtl := LoadLibrary('msvcrt.dll');
if hMsvcrtl <> 0 then
  begin
  VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
  VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
  VCMemCmp := TMSVCMemCmp(GetProcAddress(hMsvcrtl, 'memcmp'));
  end
else
  begin
  VCStrStr := nil;
  VCStrStrW := nil;
  VCMemCmp := nil;
  end;
{$ENDIF}

finalization

{$IFDEF MSWINDOWS}
if hMsvcrtl <> 0 then
  FreeLibrary(hMsvcrtl);
{$ENDIF}

end.
