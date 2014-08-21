unit qjson;
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
  * 修正了Add函数自动检测内容类型时，添加特定格式如11,23时解析出错的问题(Tuesday报告)
  2014.7.31
  =========
  * 修正了解析出错时，如果行过长，系统异常无法完整显示的问题(音儿小白报告)
  * 修正了解析时可能陷入死循环的问题（音儿小白报告）
  * 修正了出现异常时，异常行提示重复的问题
  * 修正了ForcePath时'array[].subobjectname'未正确生成路径的问题(音儿小白报告)
  2014.7.28
  =========
  * 修正了ToRtti如果源类型是日期时间类型，而JSON为null时解析出错的问题(恢宏报告)
  * 修改ToRecord参数类型为var，而不是const(恢宏报告)
  2014.7.16
  =========
  * 修正了GetPath时，未初始化结果字符串造成Path属性可能出错的问题(音儿小白报告)
  2014.7.6
  =========
  + ToRtti加入对静态数组类型的支持

  2014.7.3
  =========
  * 修正了Assign时复制了当前结点名称的问题

  2014.7.1
  =========
  * AsString修改jdtNull/jdtUnknown时，改为返回空字符串
  2014.6.28
  =========
  * 修正了ForcePath('Items[]')默认添加了空子结点的问题(pony,光明报告)
  + 加入JsonRttiEnumAsInt全局选项，控制枚举值和集合值是否保存成其字符串表达，默认为True(恢弘建议)
  2014.6.27
  =========
  + 增加TryParse函数（恢弘建议）
  * 修改了Encode时将自己的名称也加到了结果字符串中的问题（恢弘报告）
  * 修正了FromRTTI时，对于方法、事件等属性没有进行过滤的问题
  * 修正了ToRtti.ToArray时，对于动态数组的设置长度时类型错误（恢弘报告）
  2014.6.26
  ==========
  * 修正了ToRtti.ToRecord子函数处理日期类型时的错误(感谢群友恢弘大量的RTTI建议和测试)
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
  2014.6.23
  ==========
  + FromRecord支持动态数组和普通数组
  2014.6.21
  ==========
  * 移除原来AddObject/AddRecord/ToObject/ToRecord支持
  + 添加FromRtti/ToRtti/FromRecord/ToRecord/ToRttiValue函数支持，替换原来的RTTI函数
  + 添加Invoke函数，支持直接通过Json调用对应的函数，具体参考Demo
  2014.6.17
  =========
  * AsFloat赋值时加入对Nan、Infinite、NegInfinite三个无效值的检查
  * AsVariant赋值时加入对varNull,varEmpty,varUnknown,varUInt64类型的支持
  2014.5.27
  ==========
  + TQHashedJson 支持，这是一个面向查询优化的版本，使用哈希表加快ItemByName的查询速度，
  如果您的应用中大量的使用ItemByName、ItemByPath等查询，使用它代替TQJson，否则应直接
  使用TQJson

  2014.5.14
  =========
  + 加入CopyIf/DeleteIf/FindIf函数
  + 加入for..in语法支持
  * 修正了Encode和ForcePath可能存在的问题

  2014.5.6
  ========
  + 加入ParseBlock函数，以支持流式传送分段解析
  * 修正了解析\uxxxx时的识别错误
  * 修改Parse函数为清除已有子结点

  2014.5.4
  ========
  + 加入对JavaScript和.net的日期时间类型/DATE(MillSeconds+TimeZone)/格式的支持
  * Json数据支持加入VCL的TDateTime类型支持，生成的JSON数据默认由JsonDateFormat、
  JsonTimeFormat,JsonDateTimeFormat三个变量控制，如果StrictJson变量为True，则
  生成/DATE(MillSeconds+TimeZone)/格式
  【注】
  日期时间类型仅适用于运行时，生成JSON后实际上仍为字符串，这种字符串再次打开时
  将丢失类型信息，但您仍可以直接以AsDateTime属性来读写。如果日期时间类型使用
  JavaScript和.net格式并且包含了时区信息，则时间将被转换为格林威治时间。

  2014.5.1
  ========
  + 加入AddRecord函数，支持直接保存记录数据，但以下类型的成员会被忽略
  对象(Class)、函数(Method)、接口(Interface)、类型引用(ClassRef),指针(Pointer)、过程(Procedure)
  将来可能根据实际需要决定是否添加支持
  + 加入ToRecord函数，完成Json直接到记录类型的转换
  + 加入Copy函数用于创建当前结点的一个拷贝实例，注意目前版本克隆内部调用了Copy函数，将来可能改掉
  * 修正了Assign函数的一处错误
}
// 测试环境仅为Delphi 2007或XE6，其它版本的开发环境，请自行修改
uses classes, sysutils, math, qstring, typinfo, qrbtree
{$IFDEF MSWINDOWS}, windows{$ENDIF}
{$IFDEF UNICODE}, Generics.Collections, Rtti{$ENDIF}
{$IF RTLVersion<22}// 2007-2010
    , PerlRegEx, pcre
{$ELSE}
    , RegularExpressionsCore
{$IFEND}
    ;
{$M+}
{$HPPEMIT '#pragma link "qjson"'}

// 如果要使用类名来表示方式，如TForm1.FormCreate,则启用下面的定义，否则方法名为Form1.FormCreate
{ .$DEFINE TYPENAMEASMETHODPREF }
type
  /// 本单元是QDAC的组成部分，受QDAC授权限制，详情访问QDAC网站了解
  /// <summary>
  /// JSON解析单元，用于快速解析和维护JSON结构.全局变量StrictJson为False时，支持
  /// 注释和名称不包含'"'。
  /// </summary>
  /// TQJsonDataType用于记录JSON数据元的类型，可取值包括：
  /// <list>
  /// <item>
  /// <term>jdtUnknown</term><description>未知类型，只有新构造对象未赋值时，才会是该类型</description>
  /// </item>
  /// <item>
  /// <term>jdtNull</term><description>NULL</description>
  /// </item>
  /// <item>
  /// <term>jdtString</term><description>字符串</description>
  /// </item>
  /// <item>
  /// <term>jdtInteger</term><description>整数(Int64,不管整数值多大，内部均使用64位整数存贮)</description>
  /// </item>
  /// <item>
  /// <term>jdtFloat</term><description>双精度浮点数(Double)</description>
  /// </item>
  /// <item>
  /// <term>jdtBoolean</term><description>布尔</description>
  /// </item>
  /// <item>
  /// <term>jdtDateTime</term><description>日期时间类型</description>
  /// </item>
  /// <item>
  /// <term>jdtArray</term><description>数组</description>
  /// </item>
  /// <item>
  /// <term>jdtObject</term><description>对象</description>
  /// </item>
  /// </list>
  TQJsonDataType = (jdtUnknown, jdtNull, jdtString, jdtInteger, jdtFloat,
    jdtBoolean, jdtDateTime, jdtArray, jdtObject);
  TQJson = class;
{$IFDEF UNICODE}
  /// <summary>
  /// RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  /// <param name="AType">属性或字段的类型信息</param>
  /// <param name="Accept">是否记录该属性或字段</param>
  /// <param name="ATag">用户自定义的附加数据成员</param>
  TQJsonRttiFilterEventA = reference to procedure(ASender: TQJson;
    AObject: Pointer; AName: QStringW; AType: PTypeInfo; var Accept: Boolean;
    ATag: Pointer);
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQJsonFilterEventA = reference to procedure(ASender, AItem: TQJson;
    var Accept: Boolean; ATag: Pointer);
{$ENDIF UNICODE}
  /// <summary>
  /// RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  /// <param name="AType">属性或字段的类型信息</param>
  /// <param name="Accept">是否记录该属性或字段</param>
  /// <param name="ATag">用户自定义的附加数据成员</param>
  TQJsonRttiFilterEvent = procedure(ASender: TQJson; AObject: Pointer;
    AName: QStringW; AType: PTypeInfo; var Accept: Boolean; ATag: Pointer)
    of object;
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQJsonFilterEvent = procedure(ASender, AItem: TQJson; var Accept: Boolean;
    ATag: Pointer) of object;
  PQJson = ^TQJson;
{$IFDEF UNICODE}
  TQJsonItemList = TList<TQJson>;
{$ELSE}
  TQJsonItemList = TList;
{$ENDIF}
  /// <summary>
  /// TQJsonTagType用于内部AddObject和AddRecord函数的内部过滤使用
  /// </summary>
  /// <list>
  /// <item>
  /// <term>ttAnonEvent</term><description>回调匿名函数</description>
  /// <term>ttNameFilter</term><description>属性或成员名称过滤</descriptio>
  /// </list>
  TQJsonTagType = (ttAnonEvent, ttNameFilter);
  PQJsonInternalTagData = ^TQJsonInternalTagData;

  /// <summary>
  /// TQJsonInternalTagData用于AddRecord和AddObject函数需要内部过滤RTTI信息时使用
  /// </summary>
  TQJsonInternalTagData = record
    /// <summary>Tag数据的类型</summary>
    TagType: TQJsonTagType;
{$IFDEF UNICODE}
    /// <summary>过滤使用的匿名函数</summary>
    OnEvent: TQJsonRttiFilterEventA;
{$ENDIF UNICODE}
    /// <summary>接受的属性(AddObject)或记录字段(AddRecord)名称，如果名称同时在IgnoreNames出现，则IgnoreNames里的信息被忽略</summary>
    AcceptNames: QStringW;
    /// <summary>忽略的属性(AddObject)或记录字段(AddRecord)名称，如果名称同时在AcceptNameds里，则AcceptNames优先</summary>
    IgnoreNames: QStringW;
    /// <summary>原始传递给AddObject或AddRecord的附加数据成员，它将被传递给OnEvent的Tag，以供用户使用</summary>
    Tag: Pointer;
  end;

  TQJsonEnumerator = class;
  /// <summary>用于外部支持对象池的函数，创建一个新的QJSON对象，注意从池中创建的对象</summary>
  /// <returns>返回新创建的QJSON对象</returns>
  TQJsonCreateEvent = function: TQJson;
  /// <summary>用于外部将对象缓存，以便重用对象</summary>
  /// <param name="AJson">要释放的Json对象</param>
  TQJsonFreeEvent = procedure(AJson: TQJson);

  EJsonError = class(Exception)

  end;

  /// <summary>
  /// TQJson用于解析并维护JSON格式的对象类型，要使用前，需要先在堆中创建对应的实例。
  /// TQJson和TQXML在绝大多数接口上保持一致，但由于Json是有类型信息，而XML没有类型
  /// 信息（始终认为是字符串），因此少部分接口会略有不同.
  /// 与其它实现不同，QJSON所有的类型都是同一个对象实现，根据DataType的不同，而使用
  /// 不同的成员来访问。当类型为jdtArray或者是jdtObject时，它可以有子结点.
  /// </summary>
  TQJson = class
  protected
    FName: QStringW;
    FNameHash: Cardinal;
    FDataType: TQJsonDataType;
    FValue: QStringW;
    FParent: TQJson;
    FData: Pointer;
    FItems: TQJsonItemList;
    function GetValue: QStringW;
    procedure SetValue(const Value: QStringW);
    procedure SetDataType(const Value: TQJsonDataType);
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: QStringW;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: QStringW);
    function GetAsObject: QStringW;
    procedure SetAsObject(const Value: QStringW);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQJson;
    function CharUnescape(var p: PQCharW): QCharW;
    function CharEscape(c: QCharW; pd: PQCharW): Integer;
    procedure ArrayNeeded(ANewType: TQJsonDataType);
    procedure ValidArray;
    procedure ParseObject(var p: PQCharW);
    function ParseJsonPair(ABuilder: TQStringCatHelperW;
      var p: PQCharW): Integer;
    procedure BuildJsonString(ABuilder: TQStringCatHelperW; var p: PQCharW);
    function ParseName(ABuilder: TQStringCatHelperW; var p: PQCharW): Integer;
    procedure ParseValue(ABuilder: TQStringCatHelperW; var p: PQCharW);
    function FormatParseError(ACode: Integer; AMsg: QStringW; ps, p: PQCharW)
      : QStringW;
    procedure RaiseParseException(ACode: Integer; ps, p: PQCharW);
    function TryParseValue(ABuilder: TQStringCatHelperW;
      var p: PQCharW): Integer;
    function BoolToStr(const b: Boolean): QStringW;
    function GetIsNull: Boolean;
    function GetIsNumeric: Boolean;
    function GetIsArray: Boolean;
    function GetIsObject: Boolean;
    function GetIsString: Boolean;
    function GetIsDateTime: Boolean;
    function GetAsArray: QStringW;
    procedure SetAsArray(const Value: QStringW);
    function GetPath: QStringW;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetAsJson: QStringW;
    procedure SetAsJson(const Value: QStringW);
    function GetItemIndex: Integer;
    function ParseJsonTime(p: PQCharW; var ATime: TDateTime): Boolean;
    function CreateJson: TQJson; virtual;
    procedure FreeJson(AJson: TQJson); inline;
    procedure CopyValue(ASource: TQJson); inline;
    procedure Replace(AIndex: Integer; ANewItem: TQJson); virtual;
    procedure InternalRttiFilter(ASender: TQJson; AObject: Pointer;
      APropName: QStringW; APropType: PTypeInfo; var Accept: Boolean;
      ATag: Pointer);
    function InternalEncode(ABuilder: TQStringCatHelperW; ADoFormat: Boolean;
      ADoEscape: Boolean; ANullConvert: Boolean; const AIndent: QStringW)
      : TQStringCatHelperW;
    function ArrayItemTypeName(ATypeName: QStringW): QStringW;
    function ArrayItemType(ArrType: PTypeInfo): PTypeInfo;
  public
    /// <summary>构造函数</summary>
    constructor Create; overload;
    constructor Create(const AName, AValue: QStringW;
      ADataType: TQJsonDataType = jdtUnknown); overload;
    /// <summary>析构函数</summary>
    destructor Destroy; override;
    { <summary》添加一个子结点<、summary>
      <param name="ANode">要添加的结点</param>
      <returns>返回添加的结点索引</returns>
    }
    function Add(ANode: TQJson): Integer; overload;
    /// <summary>添加一个未命名的JSON子结点</summary>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>
    /// 一般情况下，除非数组类型，不应添加未命名的实例
    /// </remarks>
    function Add: TQJson; overload;
    function Add(AName, AValue: QStringW;
      ADataType: TQJsonDataType = jdtUnknown): Integer; overload;
    /// <summary>添加一个数组</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AItems">要添加的数组内容</param>
    /// <returns>返回创建的结点实例</returns>
    function Add(const AName: QStringW; AItems: array of const)
      : TQJson; overload;
    { <summary>添加一个子结点</summary>
      <param name="AName">要添加的结点名</param>
      <param name="ADataType">要添加的结点数据类型，如果省略，则自动根据值的内容检测</param>
      <returns>返回添加的新对象</returns>
      <remarks>
      1.如果当前类型不是jdtObject或jdtArray，将自动转换为jdtObject类型
      2.上层应自己负责重名检查
      </remarks>
    }
    function Add(AName: QStringW; ADataType: TQJsonDataType): TQJson; overload;

    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Extended): TQJson; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Int64): TQJson; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Boolean): TQJson; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的子结点</param>
    /// <returns>返回添加的新对象的索引位置</returns>
    /// <remarks>添加的结点释放归主本结点负责，外部不应再释放</remarks>
    function Add(AName: QStringW; AChild: TQJson): Integer; overload;
    /// <summary>添加一个数组类型子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <returns>返回添加的新对象</returns>
    function AddArray(AName: QStringW): TQJson; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function AddDateTime(AName: QStringW; AValue: TDateTime): TQJson; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function AddVariant(AName: QStringW; AValue: Variant): TQJson; overload;
    /// <summary>添加一个子结点(Null)</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW): TQJson; overload; virtual;

    /// <summary>强制一个路径存在,如果不存在,则依次创建需要的结点(jdtObject或jdtArray)</summary>
    /// <param name="APath">要添加的结点路径</param>
    /// <returns>返回路径对应的对象</returns>
    /// <remarks>
    /// 假设以下路径完全不存在，则ForcePath会按如下规则执行:
    /// 1、如果APath中包含[]，则认为对应的路径结点为数组，示例如下：
    /// (1)、'a.b[].name'：
    /// a -> jdtObject
    /// b -> jdtArray
    /// b[0].name -> jdtNull(b的索引未指定，自动认为是b[0]
    /// (2)、'a.c[2].name'：
    /// a -> jdtObject
    /// c -> jdtArray
    /// c[2].name -> jdtNull
    /// 其中,c[0],c[1]被自动创建，并赋值为jdtNull，执行完成后c为包含三个元素的数组
    /// (3)、'a[0]'：
    /// a -> jdtArray
    /// a[0] -> jdtNull
    /// 2、路径分隔符./\是等价的，并且结点名称中不应包含上述三个字符之一,即：
    /// a.b.c和a\b\c和a/b/c是完全相同的路径
    /// 3、如果APath指定的对象类型不匹配，则会抛出异常，如a为对象，但使用a[0].b访问时。
    /// </remarks>
    function ForcePath(APath: QStringW): TQJson;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name="p">要解析的字符串</param>
    /// <param name="l">字符串长度，<=0认为是以\0(#0)结尾的C语言标准字符串</param>
    /// <remarks>如果l>=0，会检测p[l]是否为\0，如果不为\0，则会创建拷贝实例并解析拷贝实例</remarks>
    procedure Parse(p: PWideChar; l: Integer = -1); overload;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name="s">要解析的JSON字符串</param>
    procedure Parse(const s: QStringW); overload;
    function TryParse(p: PWideChar; l: Integer = -1): Boolean; overload;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name="s">要解析的JSON字符串</param>
    function TryParse(const s: QStringW): Boolean; overload;
    /// <summmary>从流中解析首个JSON数据块</summary>
    /// <param name="AStream">流对象</param>
    /// <param name="AEncoding">流数据的编码方式</param>
    /// <remarks>ParseBlock适合解析分段式JSON，它会从当前位置开始，解析到当前对象结束为止.
    /// 可以很好的满足渐进式传输的需要</remarks>
    procedure ParseBlock(AStream: TStream; AEncoding: TTextEncoding);
    /// <summary>拷贝生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function Copy: TQJson;
{$IFDEF UNICODE}
    /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function CopyIf(const ATag: Pointer; AFilter: TQJsonFilterEventA)
      : TQJson; overload;
{$ENDIF UNICODE}
    /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function CopyIf(const ATag: Pointer; AFilter: TQJsonFilterEvent)
      : TQJson; overload;
    /// <summary>克隆生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为实际上执行的是拷贝，所以新旧对象之间的内容变更没有任何关系，
    /// 更改任意一个对象，不会对另外一个对象造成影响，但此行为将来并不保证，可能
    /// 会调整为引用，以便相互影响。
    /// </remarks>
    function Clone: TQJson;
    /// <summary>编码为字符串</summary>
    /// <param name="ADoFormat">是否格式化字符串，以增加可读性</param>
    /// <param name="ADoEscape">是否转义非字母和数字字符</param>
    /// <param name="AIndent">ADoFormat参数为True时，缩进内容，默认为两个空格</param>
    /// <returns>返回编码后的字符串</returns>
    /// <remarks>AsJson等价于Encode(True,'  ')</remarks>
    function Encode(ADoFormat: Boolean; ADoEscape: Boolean = False;
      AIndent: QStringW = '  '): QStringW;
    /// <summary>获取指定名称获取结点的值的字符串表示</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回应结点的值</returns>
    function ValueByName(AName, ADefVal: QStringW): QStringW;
    /// <summary>获取指定路径结点的值的字符串表示</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>如果结果不存在，返回默认值，否则，返回原始值</returns>
    function ValueByPath(APath, ADefVal: QStringW): QStringW;
    /// <summary>获取指定名称的第一个结点</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回找到的结点，如果未找到，返回空(NULL/nil)</returns>
    /// <remarks>注意QJson并不检查重名，因此，如果存在重名的结点，只会返回第一个结点</remarks>
    function ItemByName(AName: QStringW): TQJson; overload;
    /// <summary>获取指定名称的结点到列表中</summary>
    /// <param name="AName">结点名称</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByName(const AName: QStringW; AList: TQJsonItemList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取符合指定名称规则的结点到列表中</summary>
    /// <param name="ARegex">正则表达式</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByRegex(const ARegex: QStringW; AList: TQJsonItemList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取指定路径的JSON对象</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <returns>返回找到的子结点，如果未找到返回NULL(nil)</returns>
    function ItemByPath(APath: QStringW): TQJson;
    /// <summary>从源对象复制JSON对象内容</summary>
    /// <param name="ANode">要复制的源结点</param>
    /// <remarks>注意不要复制子结点给自己，否则会造成死循环。要复制子结点，先复
    /// 制一个子结点的新实例，再从新实例复制
    /// </remarks>
    procedure Assign(ANode: TQJson); virtual;
    /// <summary>删除指定索引的结点</summary>
    /// <param name="AIndex">要删除的结点索引</param>
    /// <remarks>
    /// 如果指定索引的结点不存在，则抛出EOutRange异常
    /// </remarks>
    procedure Delete(AIndex: Integer); overload; virtual;
    /// <summary>删除指定名称的结点</summary>
    /// <param name="AName">要删除的结点名称</param>
    /// <remarks>
    /// 如果要多个重名的结点，则只删除第一个
    procedure Delete(AName: QStringW); overload;
{$IFDEF UNICODE}
    /// <summary>
    /// 删除符合条件的子结点
    /// </summary>
    /// <param name="ATag">用户自己附加的额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，等价于Clear</param>
    procedure DeleteIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQJsonFilterEventA); overload;
{$ENDIF UNICODE}
    /// <summary>
    /// 删除符合条件的子结点
    /// </summary>
    /// <param name="ATag">用户自己附加的额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，等价于Clear</param>
    procedure DeleteIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQJsonFilterEvent); overload;
    /// <summary>查找指定名称的结点的索引</summary>
    /// <param name="AName">要查找的结点名称</param>
    /// <returns>返回索引值，未找到返回-1</returns>
    function IndexOf(const AName: QStringW): Integer; virtual;
{$IFDEF UNICODE}
    /// <summary>遍历结点查找符合条件的结点</summary>
    /// <param name="ATag">用户自定义的附加额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，则返回nil</param>
    function FindIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQJsonFilterEventA): TQJson; overload;
{$ENDIF UNICODE}
    /// <summary>遍历结点查找符合条件的结点</summary>
    /// <param name="ATag">用户自定义的附加额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，则返回nil</param>
    function FindIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQJsonFilterEvent): TQJson; overload;
    /// <summary>清除所有的结点</summary>
    procedure Clear; virtual;
    /// <summary>保存当前对象内容到流中</summary>
    /// <param name="AStream">目标流对象</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBom">是否写入BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToStream(AStream: TStream; AEncoding: TTextEncoding;
      AWriteBOM: Boolean);
    /// <summary>从流的当前位置开始加载JSON对象</summary>
    /// <param name="AStream">源数据流</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    /// <remarks>流的当前位置到结束的长度必需大于2字节，否则无意义</remarks>
    procedure LoadFromStream(AStream: TStream;
      AEncoding: TTextEncoding = teUnknown);
    /// <summary>保存当前对象内容到文件中</summary>
    /// <param name="AFileName">文件名</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBOM">是否写入UTF-8的BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToFile(AFileName: String; AEncoding: TTextEncoding;
      AWriteBOM: Boolean);
    /// <summary>从指定的文件中加载当前对象</summary>
    /// <param name="AFileName">要加载的文件名</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    procedure LoadFromFile(AFileName: String;
      AEncoding: TTextEncoding = teUnknown);
    /// / <summary>重置值为Null，等价于直接设置DataType为jdtNull</summary>
    procedure ResetNull;
    /// <summary>重载TObject.ToString函数</summary>
    function ToString: string; {$IFDEF UNICODE}override; {$ELSE}virtual;
{$ENDIF}
    /// <summary>获取for..in需要的GetEnumerator支持</summary>
    function GetEnumerator: TQJsonEnumerator;
    /// <summary>判断自己是否是指定对象的子对象</summmary>
    /// <param name="AParent">可能的父对象</param>
    /// <returns>如果AParent是自己的父对象，返回True，否则返回false</returns>
    function IsChildOf(AParent: TQJson): Boolean;
    /// <summary>判断自己是否是指定对象的父对象</summmary>
    /// <param name="AChild">可能的子对象</param>
    /// <returns>如果AChild是自己的子对象，返回True，否则返回false</returns>
    function IsParentOf(AChild: TQJson): Boolean;
{$IFDEF UNICODE}
    /// <summary>使用当前Json对象参数调用指定对象的相应函数</summary>
    /// <param name="AInstance">函数所隶属的对象实例</param>
    /// <returns>返回函数调用的结果</returns>
    /// <remarks>函数名称为当前结点名称，函数的参数名称与子结点的名称要保持一致</remarks>
    function Invoke(AInstance: TValue): TValue;
    /// <summary>将当前的值转换为TValue类型的值</summary>
    /// <returns>返回当前结点转换后的TValue值</returns>
    function ToRttiValue: TValue;
    /// <summary>从指定的RTTI实例中生成JSON数据</summary>
    /// <param name="AInstance">对象或其它RTTI类型值</param>
    /// <remarks>注意不是全部RTTI类型都受支持，如接口啥的</remarks>
    procedure FromRtti(AInstance: TValue); overload;
    /// <summary>将指定的来源地址按指定的类型生成JSON数据</summary>
    /// <param name="ASource">对象或结构体地址</param>
    /// <param name="AType">对象或结构体类型信息</param>
    procedure FromRtti(ASource: Pointer; AType: PTypeInfo); overload;
    /// <summary>从指定的记录实例中生成JSON数据</summary>
    /// <param name="ARecord">记录实例</param>
    procedure FromRecord<T>(const ARecord: T);
    /// <summary>从当前JSON中还原到指定的对象实例中</summary>
    /// <param name="AInstance">实例地址</param>
    /// <remarks>实际上参数只支持对象，记录由于目前无法直接转换为TValue，所以没
    /// 意义，而其它类型因为是值拷贝，实际就算赋值了也返回不了，因此无意义</remarks>
    procedure ToRtti(AInstance: TValue); overload;
    /// <summary>从当前JSON中按指定的类型信息还原到指定的地址</summary>
    /// <param name="ADest">目的地址</param>
    /// <param name="AType">对象或结构体的类型信息</param>
    /// <remarks>ADest对应的应是记录或对象，其它类型不受支持</remarks>
    procedure ToRtti(ADest: Pointer; AType: PTypeInfo); overload;
    /// <summary>从当前的JSON中还原到指定的记录实例中</summary>
    /// <param name="ARecord">目的记录实例</param>
    procedure ToRecord<T: record >(var ARecord: T);
{$ENDIF}
    /// <summary>父结点</summary>
    property Parent: TQJson read FParent;
    /// <summary>结点类型</summary>
    /// <seealso>TQJsonDataType</seealso>
    property DataType: TQJsonDataType read FDataType write SetDataType;
    /// <summary>结点名称</summary>
    property Name: QStringW read FName;
    /// <summary>子结点数量</<summary>summary>
    property Count: Integer read GetCount;
    /// <summary>子结点数组</summary>
    property Items[AIndex: Integer]: TQJson read GetItems; default;
    /// <summary>子结点的值</summary>
    property Value: QStringW read GetValue write SetValue;
    /// <summary>判断是否是NULL类型</summary>
    property IsNull: Boolean read GetIsNull;
    /// <summary>判断是否是数字类型</summary>
    property IsNumeric: Boolean read GetIsNumeric;
    /// <summary>判断是否是日期时间类型</summary>
    property IsDateTime: Boolean read GetIsDateTime;
    /// <summary>判断是否是字符串类型</summary>
    property IsString: Boolean read GetIsString;
    /// <summary>判断是否是对象</summary>
    property IsObject: Boolean read GetIsObject;
    /// <summary>判断是否是数组</summary>
    property IsArray: Boolean read GetIsArray;
    /// <summary>将当前结点当作布尔类型访问</summary>
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    /// <summary>将当前结点当作整数类型来访问</summary>
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    /// <summary>将当前结点当作64位整数类型来访问</summary>
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    /// <summary>将当前结点当作浮点类型来访问</summary>
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    /// <summary>将当前结点当作日期时间类型来访问</summary>
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    /// <summary>将当前结点当作字符串类型访问</summary>
    property AsString: QStringW read GetAsString write SetAsString;
    /// <summary>将当前结点当作一个对象字符串来访问</summary>
    property AsObject: QStringW read GetAsObject write SetAsObject;
    /// <summary>将当前结点当作一个字符串数组来访问</summary>
    property AsArray: QStringW read GetAsArray write SetAsArray;
    /// <summary>将自己当做Variant类型来访问</summary>
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    /// <summary>将自己当做Json对象来访问</summary>
    property AsJson: QStringW read GetAsJson write SetAsJson;
    // <summary>额外的附加数据成员，供用户关联附加内容</summary>
    property Data: Pointer read FData write FData;
    /// <summary>结点的路径，路径中间以"\"分隔</summary>
    property Path: QStringW read GetPath;
    /// <summary>在父结点中的索引顺序，从0开始，如果是-1，则代表自己是根结点</summary>
    property ItemIndex: Integer read GetItemIndex;
    /// <summary>名称哈希值</summary>
    property NameHash: Cardinal read FNameHash;
  end;

  TQJsonEnumerator = class
  private
    FIndex: Integer;
    FList: TQJson;
  public
    constructor Create(AList: TQJson);
    function GetCurrent: TQJson; inline;
    function MoveNext: Boolean;
    property Current: TQJson read GetCurrent;
  end;

  TQHashedJson = class(TQJson)
  protected
    FHashTable: TQHashTable;
    function CreateJson: TQJson; override;
    procedure Replace(AIndex: Integer; ANewItem: TQJson); override;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(ANode: TQJson); override;
    function Add(AName: QStringW): TQJson; override;
    function IndexOf(const AName: QStringW): Integer; override;
    procedure Delete(AIndex: Integer); override;
    procedure Clear; override;
  end;

var
  /// <summary>是否启用严格检查模式，在严格模式下：
  /// 1.名称或字符串必需使用双引号包含起来,如果为False，则名称可以没有引号或使用单引号。
  /// 2.注释不受支持，如果为False，则支持//行注释和/**/的块注释
  /// </summary>
  StrictJson: Boolean;
  /// <summary>指定如何处理RTTI中的枚举和集合类型</summary>
  JsonRttiEnumAsInt: Boolean;
  /// <summary>日期类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonDateFormat: QStringW;
  /// <summary>时间类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonTimeFormat: QStringW;
  /// <summary>日期时间类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonDateTimeFormat: QStringW;
  /// <summary>在ItemByName/ItemByPath/ValueByName/ValueByPath等函数的判断中，是否区分名称大小写</summary>
  JsonCaseSensitive: Boolean;
  /// 在需要新建一个TQJson对象时触发
  OnQJsonCreate: TQJsonCreateEvent;
  /// 在需要释放一个TQJson对象时触发
  OnQJsonFree: TQJsonFreeEvent;

implementation

uses variants, dateutils;

resourcestring
  SBadJson = '当前内容不是有效的JSON字符串。';
  SNotArrayOrObject = '%s 不是一个JSON数组或对象。';
  SVarNotArray = '%s 的类型不是数组或对象。';
  SBadConvert = '%s 不是一个有效的 %s 类型的值。';
  SCharNeeded = '当前位置应该是 "%s" ，而不是 "%s"。';
  SEndCharNeeded = '当前位置需要Json结束字符",]}"。';
  SBadNumeric = '"%s"不是有效的数值。';
  SBadJsonTime = '"%s"不是一个有效的日期时间值。';
  SBadNameStart = 'Json结点名称应以''"''字符开始。';
  SBadNameEnd = 'Json结束名称未正确结束。';
  SNameNotFound = '项目名称未找到。';
  SCommentNotSupport = '严格模式下不支持注释，要解析包括注释的JSON内容，请将StrictJson变量设置为False。';
  SUnsupportArrayItem = '添加的动态数组第%d个元素类型不受支持。';
  SBadStringStart = '严格栻上JSON字符串必需以"开始。';
  SUnknownToken = '无法识别的注释符，注释必需以//或/**/包括。';
  SNotSupport = '函数 [%s] 在当前开发环境下不受支持。';
  SBadJsonArray = '%s 不是一个有效的JSON数组定义。';
  SBadJsonObject = '%s 不是一个有效的JSON对象定义。';
  SBadJsonEncoding = '无效的编码，编码只能是UTF-8，ANSI，Unicode 16 LE，Unicode 16 BE。';
  SJsonParseError = '第%d行第%d列:%s '#13#10'行:%s';
  SBadJsonName = '%s 不是一个有效的JSON对象名称。';
  SObjectChildNeedName = '对象 %s 的第 %d 个子结点名称未赋值，编码输出前必需赋值。';
  SReplaceTypeNeed = '替换结点的类型要求是 %s 或其子类。';
  SSupportFloat = 'NaN/+∞/-∞不受JSON规范支持。';
  SParamMissed = '参数 %s 同名的结点未找到。';
  SMethodMissed = '指定的函数 %s 不存在。';
  SMissRttiTypeDefine =
    '无法找到 %s 的RTTI类型信息，尝试将对应的类型单独定义(如array[0..1] of Byte改为TByteArr=array[0..1]，然后用TByteArr声明)。';
  SUnsupportPropertyType = '不支持的属性类型。';
  SArrayTypeMissed = '未知的数组元素类型。';
  SUnknownError = '未知的错误。';

const
  JsonTypeName: array [TQJsonDataType] of QStringW = ('Unknown', 'Null',
    'String', 'Integer', 'Float', 'Boolean', 'DateTime', 'Array', 'Object');
  EParse_Unknown = -1;
  EParse_BadStringStart = 1;
  EParse_BadJson = 2;
  EParse_CommentNotSupport = 3;
  EParse_UnknownToken = 4;
  EParse_EndCharNeeded = 5;
  EParse_BadNameStart = 6;
  EParse_BadNameEnd = 7;
  EParse_NameNotFound = 8;
  { TQJson }

function TQJson.Add(AName: QStringW; AValue: Int64): TQJson;
begin
Result := Add(AName, jdtInteger);
PInt64(PQCharW(Result.FValue))^ := AValue;
end;

function TQJson.Add(AName: QStringW; AValue: Extended): TQJson;
begin
Result := Add(AName, jdtFloat);
PExtended(PQCharW(Result.FValue))^ := AValue;
end;

function TQJson.Add(AName: QStringW; AValue: Boolean): TQJson;
begin
Result := Add(AName, jdtBoolean);
PBoolean(PQCharW(Result.FValue))^ := AValue;
end;

function TQJson.Add(AName: QStringW): TQJson;
begin
Result := Add;
Result.FName := AName;
end;

function TQJson.Add(AName: QStringW; AChild: TQJson): Integer;
begin
AChild.FName := AName;
Result := Add(AChild);
end;

function TQJson.AddArray(AName: QStringW): TQJson;
begin
Result := Add(AName, jdtArray);
end;

function TQJson.AddDateTime(AName: QStringW; AValue: TDateTime): TQJson;
begin
Result := Add;
Result.FName := AName;
Result.DataType := jdtString;
Result.AsDateTime := AValue;
end;

function TQJson.AddVariant(AName: QStringW; AValue: Variant): TQJson;
begin
Result := Add(AName);
Result.AsVariant := AValue;
end;

function TQJson.Add: TQJson;
begin
Result := CreateJson;
Add(Result);
end;

function TQJson.Add(ANode: TQJson): Integer;
begin
ArrayNeeded(jdtObject);
Result := FItems.Add(ANode);
ANode.FParent := Self;
end;

function TQJson.Add(AName, AValue: QStringW; ADataType: TQJsonDataType)
  : Integer;
var
  ANode: TQJson;
  p: PQCharW;
  ABuilder: TQStringCatHelperW;
  procedure AddAsDateTime;
  var
    ATime: TDateTime;
  begin
  if ParseDateTime(PQCharW(AValue), ATime) then
    ANode.AsDateTime := ATime
  else if ParseJsonTime(PQCharW(AValue), ATime) then
    ANode.AsDateTime := ATime
  else
    raise Exception.Create(SBadJsonTime);
  end;

begin
ANode := CreateJson;
ANode.FName := AName;
Result := Add(ANode);
p := PQCharW(AValue);
if ADataType = jdtUnknown then
  begin
  ABuilder := TQStringCatHelperW.Create;
  try
    if ANode.TryParseValue(ABuilder, p) <> 0 then
      ANode.AsString := AValue
    else if p^ <> #0 then
      ANode.AsString := AValue;
  finally
    FreeObject(ABuilder);
  end;
  end
else
  begin
  case ADataType of
    jdtString:
      ANode.AsString := AValue;
    jdtInteger:
      ANode.AsInteger := StrToInt(AValue);
    jdtFloat:
      ANode.AsFloat := StrToFloat(AValue);
    jdtBoolean:
      ANode.AsBoolean := StrToBool(AValue);
    jdtDateTime:
      AddAsDateTime;
    jdtArray:
      begin
      if p^ <> '[' then
        raise Exception.CreateFmt(SBadJsonArray, [Value]);
      ANode.ParseObject(p);
      end;
    jdtObject:
      begin
      if p^ <> '{' then
        raise Exception.CreateFmt(SBadJsonObject, [Value]);
      ANode.ParseObject(p);
      end;
  end;

  end;
end;

function TQJson.Add(AName: QStringW; ADataType: TQJsonDataType): TQJson;
begin
Result := Add(AName);
Result.DataType := ADataType;
end;

function TQJson.Add(const AName: QStringW; AItems: array of const): TQJson;
var
  I: Integer;
begin
Result := Add(AName);
Result.DataType := jdtArray;
for I := Low(AItems) to High(AItems) do
  begin
  case AItems[I].VType of
    vtInteger:
      Result.Add.AsInteger := AItems[I].VInteger;
    vtBoolean:
      Result.Add.AsBoolean := AItems[I].VBoolean;
{$IFNDEF NEXTGEN}
    vtChar:
      Result.Add.AsString := QStringW(AItems[I].VChar);
{$ENDIF !NEXTGEN}
    vtExtended:
      Result.Add.AsFloat := AItems[I].VExtended^;
{$IFNDEF NEXTGEN}
    vtPChar:
      Result.Add.AsString := QStringW(AItems[I].VPChar);
    vtString:
      Result.Add.AsString := QStringW(AItems[I].VString^);
    vtAnsiString:
      Result.Add.AsString := QStringW(
{$IFDEF UNICODE}
        PAnsiString(AItems[I].VAnsiString)^
{$ELSE}
        AItems[I].VPChar
{$ENDIF UNICODE}
        );
    vtWideString:
      Result.Add.AsString := PWideString(AItems[I].VWideString)^;
{$ENDIF !NEXTGEN}
    vtPointer:
      Result.Add.AsInt64 := IntPtr(AItems[I].VPointer);
    vtWideChar:
      Result.Add.AsString := AItems[I].VWideChar;
    vtPWideChar:
      Result.Add.AsString := AItems[I].VPWideChar;
    vtCurrency:
      Result.Add.AsFloat := AItems[I].VCurrency^;
    vtInt64:
      Result.Add.AsInt64 := AItems[I].VInt64^;
{$IFDEF UNICODE}       // variants
    vtUnicodeString:
      Result.Add.AsString := AItems[I].VPWideChar;
{$ENDIF UNICODE}
    vtVariant:
      Result.Add.AsVariant := AItems[I].VVariant^;
    vtObject:
      begin
      if TObject(AItems[I].VObject) is TQJson then
        Result.Add(TObject(AItems[I].VObject) as TQJson)
      else
        raise Exception.Create(Format(SUnsupportArrayItem, [I]));
      end
  else
    raise Exception.Create(Format(SUnsupportArrayItem, [I]));
  end; // End case
  end; // End for
end;

function TQJson.ArrayItemType(ArrType: PTypeInfo): PTypeInfo;
var
  ATypeData: PTypeData;
begin
Result := nil;
if (ArrType <> nil) and (ArrType.Kind in [tkArray, tkDynArray]) then
  begin
  ATypeData := GetTypeData(ArrType);
  if (ATypeData <> nil) then
    Result := ATypeData.elType2^;
  if Result = nil then
    begin
    if ATypeData.BaseType^ = TypeInfo(Byte) then
      Result := TypeInfo(Byte);
    end;
  end;
end;

function TQJson.ArrayItemTypeName(ATypeName: QStringW): QStringW;
var
  p, ps: PQCharW;
  ACount: Integer;
begin
p := PQCharW(ATypeName);
if StartWithW(p, 'TArray<', true) then
  begin
  Inc(p, 7);
  ps := p;
  ACount := 1;
  while ACount > 0 do
    begin
    if p^ = '>' then
      Dec(ACount)
    else if p^ = '<' then
      Inc(ACount);
    Inc(p);
    end;
  Result := StrDupX(ps, p - ps - 1);
  end
else
  Result := '';
end;

procedure TQJson.ArrayNeeded(ANewType: TQJsonDataType);
begin
if not(DataType in [jdtArray, jdtObject]) then
  begin
  FDataType := ANewType;
  ValidArray;
  end;
end;

procedure TQJson.Assign(ANode: TQJson);
var
  I: Integer;
  AItem, ACopy: TQJson;
begin
if ANode.FDataType in [jdtArray, jdtObject] then
  begin
  DataType := ANode.FDataType;
  Clear;
  for I := 0 to ANode.Count - 1 do
    begin
    AItem := ANode[I];
    if Length(AItem.FName) > 0 then
      begin
      ACopy := Add(AItem.FName);
      ACopy.FNameHash := AItem.FNameHash;
      end
    else
      ACopy := Add;
    ACopy.Assign(AItem);
    end;
  end
else
  CopyValue(ANode);
end;

function TQJson.BoolToStr(const b: Boolean): QStringW;
begin
if b then
  Result := 'true'
else
  Result := 'false';
end;

procedure TQJson.BuildJsonString(ABuilder: TQStringCatHelperW; var p: PQCharW);
var
  AQuoter: QCharW;
  ps: PQCharW;
begin
ABuilder.Position := 0;
if (p^ = '"') or (p^ = '''') then
  begin
  AQuoter := p^;
  Inc(p);
  ps := p;
  while p^ <> #0 do
    begin
    if (p^ = AQuoter) then
      begin
      if ps <> p then
        ABuilder.Cat(ps, p - ps);
      if p[1] = AQuoter then
        begin
        ABuilder.Cat(AQuoter);
        Inc(p, 2);
        ps := p;
        end
      else
        begin
        Inc(p);
        SkipSpaceW(p);
        ps := p;
        Break;
        end;
      end
    else if p^ = '\' then
      begin
      if ps <> p then
        ABuilder.Cat(ps, p - ps);
      ABuilder.Cat(CharUnescape(p));
      ps := p;
      end
    else
      Inc(p);
    end;
  if ps <> p then
    ABuilder.Cat(ps, p - ps);
  end
else
  begin
  while p^ <> #0 do
    begin
    if (p^ = ':') or (p^ = ']') or (p^ = ',') or (p^ = '}') then
      Break
    else
      ABuilder.Cat(p, 1);
    Inc(p);
    end
  end;
end;

function TQJson.CharEscape(c: QCharW; pd: PQCharW): Integer;
begin
case c of
  #7:
    begin
    pd[0] := '\';
    pd[1] := 'b';
    Result := 2;
    end;
  #9:
    begin
    pd[0] := '\';
    pd[1] := 't';
    Result := 2;
    end;
  #10:
    begin
    pd[0] := '\';
    pd[1] := 'n';
    Result := 2;
    end;
  #12:
    begin
    pd[0] := '\';
    pd[1] := 'f';
    Result := 2;
    end;
  #13:
    begin
    pd[0] := '\';
    pd[1] := 'r';
    Result := 2;
    end;
  '\':
    begin
    pd[0] := '\';
    pd[1] := '\';
    Result := 2;
    end;
  '''':
    begin
    pd[0] := '\';
    pd[1] := '''';
    Result := 2;
    end;
  '"':
    begin
    pd[0] := '\';
    pd[1] := '"';
    Result := 2;
    end;
  '/':
    begin
    pd[0] := '\';
    pd[1] := '/';
    Result := 2;
    end
else
  begin
  pd[0] := c;
  Result := 1;
  end;
end;
end;

function TQJson.CharUnescape(var p: PQCharW): QCharW;
  function DecodeOrd: Integer;
  var
    c: Integer;
  begin
  Result := 0;
  c := 0;
  while (p^ <> #0) and (c < 4) do
    begin
    if IsHexChar(p^) then
      Result := (Result shl 4) + HexValue(p^)
    else
      Break;
    Inc(p);
    Inc(c);
    end
  end;

begin
if p^ = #0 then
  begin
  Result := #0;
  Exit;
  end;
if p^ <> '\' then
  begin
  Result := p^;
  Inc(p);
  Exit;
  end;
Inc(p);
case p^ of
  'b':
    begin
    Result := #7;
    Inc(p);
    end;
  't':
    begin
    Result := #9;
    Inc(p);
    end;
  'n':
    begin
    Result := #10;
    Inc(p);
    end;
  'f':
    begin
    Result := #12;
    Inc(p);
    end;
  'r':
    begin
    Result := #13;
    Inc(p);
    end;
  '\':
    begin
    Result := '\';
    Inc(p);
    end;
  '''':
    begin
    Result := '''';
    Inc(p);
    end;
  '"':
    begin
    Result := '"';
    Inc(p);
    end;
  'u':
    begin
    // \uXXXX
    if IsHexChar(p[1]) and IsHexChar(p[2]) and IsHexChar(p[3]) and
      IsHexChar(p[4]) then
      begin
      Result := WideChar((HexValue(p[1]) shl 12) or (HexValue(p[2]) shl 8) or
        (HexValue(p[3]) shl 4) or HexValue(p[4]));
      Inc(p, 5);
      end
    else
      raise Exception.CreateFmt(SCharNeeded, ['0-9A-Fa-f', StrDupW(p, 0, 4)]);
    end;
  '/':
    begin
    Result := '/';
    Inc(p);
    end
else
  begin
  if StrictJson then
    raise Exception.CreateFmt(SCharNeeded, ['btfrn"u''/', StrDupW(p, 0, 4)])
  else
    begin
    Result := p^;
    Inc(p);
    end;
  end;
end;
end;

procedure TQJson.Clear;
var
  I: Integer;
begin
if FDataType in [jdtArray, jdtObject] then
  begin
  for I := 0 to Count - 1 do
    FreeJson(FItems[I]);
  FItems.Clear;
  end;
end;

function TQJson.Clone: TQJson;
begin
Result := Copy;
end;

function TQJson.Copy: TQJson;
begin
Result := CreateJson;
Result.Assign(Self);
end;
{$IFDEF UNICODE}

function TQJson.CopyIf(const ATag: Pointer;
  AFilter: TQJsonFilterEventA): TQJson;
  procedure NestCopy(AParentSource, AParentDest: TQJson);
  var
    I: Integer;
    Accept: Boolean;
    AChildSource, AChildDest: TQJson;
  begin
  for I := 0 to AParentSource.Count - 1 do
    begin
    Accept := true;
    AChildSource := AParentSource[I];
    AFilter(Self, AChildSource, Accept, ATag);
    if Accept then
      begin
      AChildDest := AParentDest.Add(AChildSource.FName, AChildSource.DataType);
      if AChildSource.DataType in [jdtArray, jdtObject] then
        begin
        AChildDest.DataType := AChildSource.DataType;
        NestCopy(AChildSource, AChildDest)
        end
      else
        AChildDest.CopyValue(AChildSource);
      end;
    end;
  end;

begin
if Assigned(AFilter) then
  begin
  Result := CreateJson;
  Result.FName := Name;
  if DataType in [jdtObject, jdtArray] then
    begin
    NestCopy(Self, Result);
    end
  else
    Result.CopyValue(Self);
  end
else
  Result := Copy;
end;
{$ENDIF UNICODE}

function TQJson.CopyIf(const ATag: Pointer; AFilter: TQJsonFilterEvent): TQJson;
  procedure NestCopy(AParentSource, AParentDest: TQJson);
  var
    I: Integer;
    Accept: Boolean;
    AChildSource, AChildDest: TQJson;
  begin
  for I := 0 to AParentSource.Count - 1 do
    begin
    Accept := true;
    AChildSource := AParentSource[I];
    AFilter(Self, AChildSource, Accept, ATag);
    if Accept then
      begin
      AChildDest := AParentDest.Add(AChildSource.FName, AChildSource.DataType);
      if AChildSource.DataType in [jdtArray, jdtObject] then
        NestCopy(AChildSource, AChildDest)
      else
        AChildDest.CopyValue(AChildSource);
      end;
    end;
  end;

begin
if Assigned(AFilter) then
  begin
  Result := CreateJson;
  Result.FName := Name;
  if DataType in [jdtObject, jdtArray] then
    begin
    NestCopy(Self, Result);
    end
  else
    Result.CopyValue(Self);
  end
else
  Result := Copy;
end;

procedure TQJson.CopyValue(ASource: TQJson);
var
  l: Integer;
begin
l := Length(ASource.FValue);
DataType := ASource.DataType;
SetLength(FValue, l);
if l > 0 then
  Move(PQCharW(ASource.FValue)^, PQCharW(FValue)^, l shl 1);
end;

constructor TQJson.Create(const AName, AValue: QStringW;
  ADataType: TQJsonDataType);
begin
inherited Create;
FName := AName;
if ADataType <> jdtUnknown then
  DataType := ADataType;
Value := AValue;
end;

function TQJson.CreateJson: TQJson;
begin
if Assigned(OnQJsonCreate) then
  Result := OnQJsonCreate
else
  Result := TQJson.Create;
end;

constructor TQJson.Create;
begin
inherited;
end;

procedure TQJson.Delete(AName: QStringW);
var
  I: Integer;
begin
I := IndexOf(AName);
if I <> -1 then
  Delete(I);
end;
{$IFDEF UNICODE}

procedure TQJson.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEventA);
  procedure DeleteChildren(AParent: TQJson);
  var
    I: Integer;
    Accept: Boolean;
    AChild: TQJson;
  begin
  I := 0;
  while I < AParent.Count do
    begin
    Accept := true;
    AChild := AParent.Items[I];
    if ANest then
      DeleteChildren(AChild);
    AFilter(Self, AChild, Accept, ATag);
    if Accept then
      AParent.Delete(I)
    else
      Inc(I);
    end;
  end;

begin
if Assigned(AFilter) then
  DeleteChildren(Self)
else
  Clear;
end;
{$ENDIF UNICODE}

procedure TQJson.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEvent);
  procedure DeleteChildren(AParent: TQJson);
  var
    I: Integer;
    Accept: Boolean;
    AChild: TQJson;
  begin
  I := 0;
  while I < AParent.Count do
    begin
    Accept := true;
    AChild := AParent.Items[I];
    if ANest then
      DeleteChildren(AChild);
    AFilter(Self, AChild, Accept, ATag);
    if Accept then
      AParent.Delete(I)
    else
      Inc(I);
    end;
  end;

begin
if Assigned(AFilter) then
  DeleteChildren(Self)
else
  Clear;
end;

procedure TQJson.Delete(AIndex: Integer);
begin
if FDataType in [jdtArray, jdtObject] then
  begin
  FreeJson(Items[AIndex]);
  FItems.Delete(AIndex);
  end
else
  raise Exception.Create(Format(SNotArrayOrObject, [FName]));
end;

destructor TQJson.Destroy;
begin
if DataType in [jdtArray, jdtObject] then
  begin
  Clear;
  FreeObject(FItems);
  end;
inherited;
end;

function TQJson.Encode(ADoFormat: Boolean; ADoEscape: Boolean;
  AIndent: QStringW): QStringW;
var
  ABuilder: TQStringCatHelperW;
begin
ABuilder := TQStringCatHelperW.Create;
try
  InternalEncode(ABuilder, ADoFormat, ADoEscape, False, AIndent);
  ABuilder.Back(1); // 删除最后一个逗号
  Result := ABuilder.Value;
finally
  FreeObject(ABuilder);
end;
end;
{$IFDEF UNICODE}

function TQJson.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEventA): TQJson;
  function DoFind(AParent: TQJson): TQJson;
  var
    I: Integer;
    AChild: TQJson;
    Accept: Boolean;
  begin
  Result := nil;
  for I := 0 to AParent.Count - 1 do
    begin
    AChild := AParent[I];
    Accept := true;
    AFilter(Self, AChild, Accept, ATag);
    if Accept then
      Result := AChild
    else if ANest then
      Result := DoFind(AChild);
    if Result <> nil then
      Break;
    end;
  end;

begin
if Assigned(AFilter) then
  Result := DoFind(Self)
else
  Result := nil;
end;
{$ENDIF UNICODE}

function TQJson.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEvent): TQJson;
  function DoFind(AParent: TQJson): TQJson;
  var
    I: Integer;
    AChild: TQJson;
    Accept: Boolean;
  begin
  Result := nil;
  for I := 0 to AParent.Count - 1 do
    begin
    AChild := AParent[I];
    Accept := true;
    AFilter(Self, AChild, Accept, ATag);
    if Accept then
      Result := AChild
    else if ANest then
      Result := DoFind(AChild);
    if Result <> nil then
      Break;
    end;
  end;

begin
if Assigned(AFilter) then
  Result := DoFind(Self)
else
  Result := nil;
end;

function TQJson.ForcePath(APath: QStringW): TQJson;
var
  AName: QStringW;
  p, pn, ws: PQCharW;
  AParent: TQJson;
  l: Integer;
  AIndex: Int64;
const
  PathDelimiters: PWideChar = './\';
begin
p := PQCharW(APath);
AParent := Self;
Result := Self;
while p^ <> #0 do
  begin
  AName := DecodeTokenW(p, PathDelimiters, WideChar(0), true);
  if not(AParent.DataType in [jdtObject, jdtArray]) then
    AParent.DataType := jdtObject;
  Result := AParent.ItemByName(AName);
  if not Assigned(Result) then
    begin
    pn := PQCharW(AName);
    l := Length(AName);
    AIndex := -1;
    if (pn[l - 1] = ']') then
      begin
      repeat
        if pn[l] = '[' then
          begin
          ws := pn + l + 1;
          if ParseInt(ws, AIndex) = 0 then
            AIndex := -1;
          Break;
          end
        else
          Dec(l);
      until l = 0;
      if l > 0 then
        begin
        AName := StrDupX(pn, l);
        Result := AParent.ItemByName(AName);
        if Result = nil then
          Result := AParent.Add(AName, jdtArray)
        else if Result.DataType <> jdtArray then
          raise Exception.CreateFmt(SBadJsonArray, [AName]);
        if AIndex >= 0 then
          begin
          while Result.Count <= AIndex do
            Result.Add;
          Result := Result[AIndex];
          end;
        end
      else
        raise Exception.CreateFmt(SBadJsonName, [AName]);
      end
    else
      begin
      if AParent.IsArray then
        Result := AParent.Add.Add(AName)
      else
        Result := AParent.Add(AName);
      end;
    end;
  AParent := Result;
  end;
end;

function TQJson.FormatParseError(ACode: Integer; AMsg: QStringW; ps, p: PQCharW)
  : QStringW;
var
  ACol, ARow: Integer;
  ALine: QStringW;
begin
if ACode <> 0 then
  begin
  p := StrPosW(ps, p, ACol, ARow);
  ALine := DecodeLineW(p, False);
  if Length(ALine) > 1024 then // 一行最长允许1024个字符
    begin
    SetLength(ALine, 1024);
    PQCharW(ALine)[1023] := '.';
    PQCharW(ALine)[1022] := '.';
    PQCharW(ALine)[1021] := '.';
    end;
  Result := Format(SJsonParseError, [ARow, ACol, AMsg, ALine]);
  end
else
  SetLength(Result, 0);
end;

procedure TQJson.FreeJson(AJson: TQJson);
begin
if Assigned(OnQJsonFree) then
  OnQJsonFree(AJson)
else
  FreeObject(AJson);
end;
{$IFDEF UNICODE}

procedure TQJson.FromRecord<T>(const ARecord: T);
begin
FromRtti(@ARecord, TypeInfo(T));
end;

procedure TQJson.FromRtti(ASource: Pointer; AType: PTypeInfo);
var
  AValue: TValue;
  procedure AddCollection(AParent: TQJson; ACollection: TCollection);
  var
    J: Integer;
  begin
  for J := 0 to ACollection.Count - 1 do
    AParent.Add.FromRtti(ACollection.Items[J]);
  end;
// 修正XE6中System.rtti中TValue对tkSet类型处理的Bug
  function SetAsOrd(AValue: TValue): Int64;
  var
    ATemp: Integer;
  begin
  AValue.ExtractRawData(@ATemp);
  case GetTypeData(AValue.TypeInfo).OrdType of
    otSByte:
      Result := PShortint(@ATemp)^;
    otUByte:
      Result := PByte(@ATemp)^;
    otSWord:
      Result := PSmallint(@ATemp)^;
    otUWord:
      Result := PWord(@ATemp)^;
    otSLong:
      Result := PInteger(@ATemp)^;
    otULong:
      Result := PCardinal(@ATemp)^
  else
    Result := 0;
  end;
  end;
  procedure AddRecord;
  var
    AContext: TRttiContext;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    I, J: Integer;
    AObj: TObject;
  begin
  AContext := TRttiContext.Create;
  ARttiType := AContext.GetType(AType);
  AFields := ARttiType.GetFields;
  for J := Low(AFields) to High(AFields) do
    begin
    if AFields[J].FieldType <> nil then
      begin
      // 如果是从结构体，则记录其成员，如果是对象，则只记录其公开的属性，特殊处理TStrings和TCollection
      case AFields[J].FieldType.TypeKind of
        tkInteger:
          Add(AFields[J].Name).AsInteger := AFields[J].GetValue(ASource)
            .AsInteger;
{$IFNDEF NEXTGEN}tkString, tkLString, tkWString,
{$ENDIF !NEXTGEN}tkUString:
          Add(AFields[J].Name).AsString := AFields[J].GetValue(ASource)
            .AsString;
        tkEnumeration:
          begin
          if GetTypeData(AFields[J].FieldType.Handle)
            .BaseType^ = TypeInfo(Boolean) then
            Add(AFields[J].Name).AsBoolean := AFields[J].GetValue(ASource)
              .AsBoolean
          else if JsonRttiEnumAsInt then
            Add(AFields[J].Name).AsInteger := AFields[J].GetValue(ASource)
              .AsOrdinal
          else
            Add(AFields[J].Name).AsString :=
              AFields[J].GetValue(ASource).ToString;
          end;
        tkSet:
          begin
          if JsonRttiEnumAsInt then
            Add(AFields[J].Name).AsInt64 :=
              SetAsOrd(AFields[J].GetValue(ASource))
          else
            Add(AFields[J].Name).AsString :=
              AFields[J].GetValue(ASource).ToString;
          end;
        tkChar, tkWChar:
          Add(AFields[J].Name).AsString := AFields[J].GetValue(ASource)
            .ToString;
        tkFloat:
          begin
          if (AFields[J].FieldType.Handle = TypeInfo(TDateTime)) or
            (AFields[J].FieldType.Handle = TypeInfo(TTime)) or
            (AFields[J].FieldType.Handle = TypeInfo(TDate)) then
            Add(AFields[J].Name).AsDateTime := AFields[J].GetValue(ASource)
              .AsExtended
          else
            Add(AFields[J].Name).AsFloat := AFields[J].GetValue(ASource)
              .AsExtended;
          end;
        tkInt64:
          Add(AFields[J].Name).AsInt64 := AFields[J].GetValue(ASource).AsInt64;
        tkVariant:
          Add(AFields[J].Name).AsVariant := AFields[J].GetValue(ASource)
            .AsVariant;
        tkArray, tkDynArray:
          begin
          with Add(AFields[J].Name, jdtArray) do
            begin
            AValue := AFields[J].GetValue(ASource);
            for I := 0 to AValue.GetArrayLength - 1 do
              Add.FromRtti(AValue.GetArrayElement(I));
            end;
          end;
        tkClass:
          begin
          AValue := AFields[J].GetValue(ASource);
          AObj := AValue.AsObject;
          if (AObj is TStrings) then
            Add(AFields[J].Name).AsString := TStrings(AObj).Text
          else if AObj is TCollection then
            AddCollection(Add(AFields[J].Name, jdtArray), AObj as TCollection)
          else // 其它类型的对象不保存
            Add(AFields[J].Name).FromRtti(AObj, AFields[J].FieldType.Handle);
          end;
        tkRecord:
          begin
          DataType := jdtObject;
          AValue := AFields[J].GetValue(ASource);
          Add(AFields[J].Name)
            .FromRtti(Pointer(IntPtr(ASource) + AFields[J].Offset),
            AFields[J].FieldType.Handle);
          end;
      end;
      end
    else
      raise Exception.CreateFmt(SMissRttiTypeDefine, [AFields[J].Name]);
    end;
  end;

  procedure AddObject;
  var
    APropList: PPropList;
    ACount: Integer;
    J: Integer;
    AObj, AChildObj: TObject;
    AName: String;
  begin
  AObj := ASource;
  if AObj is TStrings then
    AsString := (AObj as TStrings).Text
  else if AObj is TCollection then
    AddCollection(Self, AObj as TCollection)
  else
    begin
    ACount := GetPropList(AType, APropList);
    for J := 0 to ACount - 1 do
      begin
      if not((APropList[J].PropType^.Kind in [tkMethod, tkInterface, tkClassRef,
        tkPointer, tkProcedure]) or IsDefaultPropertyValue(AObj, APropList[J],
        nil)) then
        begin
{$IF RTLVersion>25}
        AName := APropList[J].NameFld.ToString;
{$ELSE}
        AName := String(APropList[J].Name);
{$IFEND}
        case APropList[J].PropType^.Kind of
          tkClass:
            begin
            AChildObj := Pointer(GetOrdProp(AObj, APropList[J]));
            if AChildObj is TStrings then
              Add(AName).AsString := (AChildObj as TStrings).Text
            else if AChildObj is TCollection then
              AddCollection(Add(AName), AChildObj as TCollection)
            else
              Add(AName).FromRtti(AChildObj);
            end;
          tkRecord, tkArray, tkDynArray: // 记录、数组、动态数组属性系统也不保存，也没提供所有太好的接口
            raise Exception.Create(SUnsupportPropertyType);
          tkInteger:
            Add(AName).AsInt64 := GetOrdProp(AObj, APropList[J]);
          tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
            Add(AName).AsString := GetStrProp(AObj, APropList[J]);
          tkEnumeration:
            begin
            if GetTypeData(APropList[J]^.PropType^)^.BaseType^ = TypeInfo
              (Boolean) then
              Add(AName).AsBoolean := GetOrdProp(AObj, APropList[J]) <> 0
            else if JsonRttiEnumAsInt then
              Add(AName).AsInteger := GetOrdProp(AObj, APropList[J])
            else
              Add(AName).AsString := GetEnumProp(AObj, APropList[J]);
            end;
          tkSet:
            begin
            if JsonRttiEnumAsInt then
              Add(Name).AsInteger := GetOrdProp(AObj, APropList[J])
            else
              Add(AName).AsString := GetSetProp(AObj, APropList[J], true);
            end;
          tkVariant:
            Add(AName).AsVariant := GetPropValue(AObj, APropList[J]);
          tkInt64:
            Add(AName).AsInt64 := GetInt64Prop(AObj, APropList[J]);
        end;
        end;
      end;
    FreeMem(APropList);
    end;
  end;

  procedure AddArray;
  var
    I, c: Integer;
  begin
  DataType := jdtArray;
  Clear;
  TValue.Make(ASource, AType, AValue);
  c := AValue.GetArrayLength;
  for I := 0 to c - 1 do
    Add.FromRtti(AValue.GetArrayElement(I));
  end;

begin
if ASource = nil then
  Exit;
case AType.Kind of
  tkRecord:
    AddRecord;
  tkClass:
    AddObject;
  tkDynArray:
    AddArray;
end;
end;

procedure TQJson.FromRtti(AInstance: TValue);
var
  I, c: Integer;
begin
case AInstance.Kind of
  tkClass:
    FromRtti(AInstance.AsObject, AInstance.TypeInfo);
  tkRecord:
    FromRtti(AInstance.GetReferenceToRawData, AInstance.TypeInfo);
  tkArray, tkDynArray:
    begin
    DataType := jdtArray;
    Clear;
    c := AInstance.GetArrayLength;
    for I := 0 to c - 1 do
      Add.FromRtti(AInstance.GetArrayElement(I));
    end;
  tkInteger:
    AsInt64 := AInstance.AsInt64;
  tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
    AsString := AInstance.ToString;
  tkEnumeration:
    begin
    if GetTypeData(AInstance.TypeInfo)^.BaseType^ = TypeInfo(Boolean) then
      AsBoolean := AInstance.AsBoolean
    else if JsonRttiEnumAsInt then
      AsInteger := AInstance.AsOrdinal
    else
      AsString := AInstance.ToString;
    end;
  tkSet:
    AsString := AInstance.ToString;
  tkVariant:
    AsVariant := AInstance.AsVariant;
  tkInt64:
    AsInt64 := AInstance.AsInt64;
end;
end;
{$ENDIF UNICODE}

function TQJson.GetAsArray: QStringW;
begin
if DataType = jdtArray then
  Result := Value
else
  raise Exception.Create(Format(SBadConvert, [AsString, 'Array']));
end;

function TQJson.GetAsBoolean: Boolean;
begin
if DataType = jdtBoolean then
  Result := PBoolean(FValue)^
else if DataType = jdtString then
  begin
  if not TryStrToBool(FValue, Result) then
    raise Exception.Create(Format(SBadConvert, [FValue, 'Boolean']));
  end
else if DataType in [jdtFloat, jdtDateTime] then
  Result := not SameValue(AsFloat, 0)
else if DataType = jdtInteger then
  Result := AsInt64 <> 0
else
  raise Exception.Create(Format(SBadConvert, [JsonTypeName[DataType],
    'Boolean']));
end;

function TQJson.GetAsDateTime: TDateTime;
begin
if DataType in [jdtDateTime, jdtFloat] then
  Result := PExtended(FValue)^
else if DataType = jdtString then
  begin
  if not(ParseDateTime(PWideChar(FValue), Result) or
    ParseJsonTime(PWideChar(FValue), Result) or ParseWebTime(PQCharW(FValue),
    Result)) then
    raise Exception.Create(Format(SBadConvert, ['String', 'DateTime']))
  end
else if DataType = jdtInteger then
  Result := AsInt64
else if DataType in [jdtNull, jdtUnknown] then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert, [JsonTypeName[DataType],
    'DateTime']));
end;

function TQJson.GetAsFloat: Extended;
begin
if DataType in [jdtFloat, jdtDateTime] then
  Result := PExtended(FValue)^
else if DataType = jdtBoolean then
  Result := Integer(AsBoolean)
else if DataType = jdtString then
  begin
  if not TryStrToFloat(FValue, Result) then
    raise Exception.Create(Format(SBadConvert, [FValue, 'Numeric']));
  end
else if DataType = jdtInteger then
  Result := AsInt64
else if DataType = jdtNull then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert, [JsonTypeName[DataType],
    'Numeric']))
end;

function TQJson.GetAsInt64: Int64;
begin
if DataType = jdtInteger then
  Result := PInt64(FValue)^
else if DataType in [jdtFloat, jdtDateTime] then
  Result := Trunc(PExtended(FValue)^)
else if DataType = jdtBoolean then
  Result := Integer(AsBoolean)
else if DataType = jdtString then
  Result := Trunc(AsFloat)
else if DataType = jdtNull then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert, [JsonTypeName[DataType],
    'Numeric']))
end;

function TQJson.GetAsInteger: Integer;
begin
Result := GetAsInt64;
end;

function TQJson.GetAsJson: QStringW;
begin
Result := Encode(true, False, '  ');
end;

function TQJson.GetAsObject: QStringW;
begin
if DataType = jdtObject then
  Result := Value
else
  raise Exception.Create(Format(SBadConvert, [AsString, 'Object']));
end;

function TQJson.GetAsString: QStringW;
begin
if DataType in [jdtNull, jdtUnknown] then
  SetLength(Result, 0)
else
  Result := Value;
end;

function TQJson.GetAsVariant: Variant;
var
  I: Integer;
begin
case DataType of
  jdtString:
    Result := AsString;
  jdtInteger:
    Result := AsInt64;
  jdtFloat:
    Result := AsFloat;
  jdtDateTime:
    Result := AsDateTime;
  jdtBoolean:
    Result := AsBoolean;
  jdtArray, jdtObject:
    begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := Items[I].AsVariant;
    end
else
  VarClear(Result);
end;
end;

function TQJson.GetCount: Integer;
begin
if DataType in [jdtObject, jdtArray] then
  Result := FItems.Count
else
  Result := 0;
end;

function TQJson.GetEnumerator: TQJsonEnumerator;
begin
Result := TQJsonEnumerator.Create(Self);
end;

function TQJson.GetIsArray: Boolean;
begin
Result := (DataType = jdtArray);
end;

function TQJson.GetIsDateTime: Boolean;
var
  ATime: TDateTime;
begin
Result := (DataType = jdtDateTime);
if not Result then
  begin
  if DataType = jdtString then
    Result := ParseDateTime(PQCharW(FValue), ATime) or
      ParseJsonTime(PQCharW(FValue), ATime) or
      ParseWebTime(PQCharW(FValue), ATime);
  end;
end;

function TQJson.GetIsNull: Boolean;
begin
Result := (DataType = jdtNull);
end;

function TQJson.GetIsNumeric: Boolean;
begin
Result := (DataType in [jdtInteger, jdtFloat]);
end;

function TQJson.GetIsObject: Boolean;
begin
Result := (DataType = jdtObject);
end;

function TQJson.GetIsString: Boolean;
begin
Result := (DataType = jdtString);
end;

function TQJson.GetItemIndex: Integer;
var
  I: Integer;
begin
Result := -1;
if Assigned(Parent) then
  begin
  for I := 0 to Parent.Count - 1 do
    begin
    if Parent.Items[I] = Self then
      begin
      Result := I;
      Break;
      end;
    end;
  end;
end;

function TQJson.GetItems(AIndex: Integer): TQJson;
begin
Result := FItems[AIndex];
end;

function TQJson.GetPath: QStringW;
var
  AParent, AItem: TQJson;
begin
AParent := FParent;
AItem := Self;
SetLength(Result, 0);
repeat
  if Assigned(AParent) and AParent.IsArray then
    Result := '[' + IntToStr(AItem.ItemIndex) + ']' + Result
  else if AItem.IsArray then
    Result := '\' + AItem.FName + Result
  else
    Result := '\' + AItem.FName + Result;
  if AParent <> nil then
    begin
    AItem := AParent;
    AParent := AItem.Parent;
    end;
until AParent = nil;
if Length(Result) > 0 then
  Result := StrDupX(PQCharW(Result) + 1, Length(Result) - 1);
end;

function TQJson.GetValue: QStringW;
  procedure ValueAsDateTime;
  var
    ADate: Integer;
    AValue: Extended;
  begin
  AValue := PExtended(FValue)^;
  ADate := Trunc(AValue);
  if SameValue(ADate, 0) then // Date为0，是时间
    begin
    if SameValue(AValue, 0) then
      Result := FormatDateTime(JsonDateFormat, AValue)
    else
      Result := FormatDateTime(JsonTimeFormat, AValue);
    end
  else
    begin
    if SameValue(AValue - ADate, 0) then
      Result := FormatDateTime(JsonDateFormat, AValue)
    else
      Result := FormatDateTime(JsonDateTimeFormat, AValue);
    end;
  end;

begin
case DataType of
  jdtNull, jdtUnknown:
    Result := 'null';
  jdtString:
    Result := FValue;
  jdtInteger:
    Result := IntToStr(PInt64(FValue)^);
  jdtFloat:
    Result := FloatToStr(PExtended(FValue)^);
  jdtDateTime:
    ValueAsDateTime;
  jdtBoolean:
    Result := BoolToStr(PBoolean(FValue)^);
  jdtArray, jdtObject:
    Result := Encode(true);
end;
end;

function TQJson.IndexOf(const AName: QStringW): Integer;
var
  I, l: Integer;
  AItem: TQJson;
  AHash: Cardinal;
begin
Result := -1;
l := Length(AName);
if l > 0 then
  AHash := HashOf(PQCharW(AName), l shl 1)
else
  AHash := 0;
for I := 0 to Count - 1 do
  begin
  AItem := Items[I];
  if Length(AItem.FName) = l then
    begin
    if JsonCaseSensitive then
      begin
      if AItem.FNameHash = 0 then
        AItem.FNameHash := HashOf(PQCharW(AItem.FName), l shl 1);
      if AItem.FNameHash = AHash then
        begin
        if AItem.FName = AName then
          begin
          Result := I;
          Break;
          end;
        end;
      end
    else if StartWithW(PQCharW(AItem.FName), PQCharW(AName), true) then
      begin
      Result := I;
      Break;
      end;
    end;
  end;
end;

function TQJson.InternalEncode(ABuilder: TQStringCatHelperW; ADoFormat: Boolean;
  ADoEscape: Boolean; ANullConvert: Boolean; const AIndent: QStringW)
  : TQStringCatHelperW;
  procedure CatValue(const AValue: QStringW);
  var
    ps: PQCharW;
  const
    CharNum1: PWideChar = '1';
    CharNum0: PWideChar = '0';
    Char7: PWideChar = '\b';
    Char9: PWideChar = '\t';
    Char10: PWideChar = '\n';
    Char12: PWideChar = '\f';
    Char13: PWideChar = '\r';
    CharQuoter: PWideChar = '\"';
    CharBackslash: PWideChar = '\\';
    CharCode: PWideChar = '\u00';
    CharEscape: PWideChar = '\u';
  begin
  ps := PQCharW(AValue);
  while ps^ <> #0 do
    begin
    case ps^ of
      #7:
        ABuilder.Cat(Char7, 2);
      #9:
        ABuilder.Cat(Char9, 2);
      #10:
        ABuilder.Cat(Char10, 2);
      #12:
        ABuilder.Cat(Char12, 2);
      #13:
        ABuilder.Cat(Char13, 2);
      '\':
        ABuilder.Cat(CharBackslash, 2);
      '"':
        ABuilder.Cat(CharQuoter, 2);
    else
      begin
      if ps^ < #$1F then
        begin
        ABuilder.Cat(CharCode, 4);
        if ps^ > #$F then
          ABuilder.Cat(CharNum1, 1)
        else
          ABuilder.Cat(CharNum0, 1);
        ABuilder.Cat(HexChar(Ord(ps^) and $0F));
        end
      else if (ps^ <= #$7E) or (not ADoEscape) then // 英文字符区
        ABuilder.Cat(ps, 1)
      else
        ABuilder.Cat(CharEscape, 2).Cat(HexChar((PWord(ps)^ shr 12) and $0F))
          .Cat(HexChar((PWord(ps)^ shr 8) and $0F))
          .Cat(HexChar((PWord(ps)^ shr 4) and $0F))
          .Cat(HexChar(PWord(ps)^ and $0F));
      end;
    end;
    Inc(ps);
    end;
  end;

  procedure StrictJsonTime(ATime: TDateTime);
  var
    MS: Int64; // 时区信息不保存
  const
    JsonTimeStart: PWideChar = '"/DATE(';
    JsonTimeEnd: PWideChar = ')/"';
  begin
  MS := Trunc(ATime * 86400000);
  ABuilder.Cat(JsonTimeStart, 7);
  ABuilder.Cat(IntToStr(MS));
  ABuilder.Cat(JsonTimeEnd, 3);
  end;

  procedure DoEncode(ANode: TQJson; ALevel: Integer);
  var
    I: Integer;
    ArrayWraped: Boolean;
    AChild: TQJson;
  const
    CharStringStart: PWideChar = '"';
    CharStringEnd: PWideChar = '",';
    CharNameEnd: PWideChar = '":';
    CharArrayStart: PWideChar = '[';
    CharArrayEnd: PWideChar = '],';
    CharObjectStart: PWideChar = '{';
    CharObjectEnd: PWideChar = '},';
    CharNull: PWideChar = 'null,';
    CharFalse: PWideChar = 'false,';
    CharTrue: PWideChar = 'true,';
    CharComma: PWideChar = ',';
  begin
  if (ANode.Parent <> nil) and (ANode.Parent.DataType <> jdtArray) and
    (Length(ANode.FName) > 0) and (ANode <> Self) then
    begin
    if ADoFormat then
      ABuilder.Replicate(AIndent, ALevel);
    ABuilder.Cat(CharStringStart, 1);
    CatValue(ANode.FName);
    ABuilder.Cat(CharNameEnd, 2);
    end;
  case ANode.DataType of
    jdtArray:
      begin
      ABuilder.Cat(CharArrayStart, 1);
      if ANode.Count > 0 then
        begin
        ArrayWraped := False;
        for I := 0 to ANode.Count - 1 do
          begin
          AChild := ANode.Items[I];
          if AChild.DataType in [jdtArray, jdtObject] then
            begin
            if ADoFormat then
              begin
              ABuilder.Cat(SLineBreak); // 对于对象和数组，换行
              ABuilder.Replicate(AIndent, ALevel + 1);
              ArrayWraped := true;
              end;
            end;
          DoEncode(AChild, ALevel + 1);
          end;
        ABuilder.Back(1);
        if ArrayWraped then
          begin
          ABuilder.Cat(SLineBreak);
          ABuilder.Replicate(AIndent, ALevel);
          end;
        end;
      ABuilder.Cat(CharArrayEnd, 2);
      end;
    jdtObject:
      begin
      if ADoFormat then
        begin
        ABuilder.Cat(CharObjectStart, 1);
        ABuilder.Cat(SLineBreak);
        end
      else
        ABuilder.Cat(CharObjectStart, 1);
      if ANode.Count > 0 then
        begin
        for I := 0 to ANode.Count - 1 do
          begin
          AChild := ANode.Items[I];
          if Length(AChild.Name) = 0 then
            raise Exception.CreateFmt(SObjectChildNeedName, [ANode.Name, I]);
          DoEncode(AChild, ALevel + 1);
          if ADoFormat then
            ABuilder.Cat(SLineBreak);
          end;
        if ADoFormat then
          ABuilder.Back(Length(SLineBreak) + 1)
        else
          ABuilder.Back(1);
        end;
      if ADoFormat then
        begin
        ABuilder.Cat(SLineBreak);
        ABuilder.Replicate(AIndent, ALevel);
        end;
      ABuilder.Cat(CharObjectEnd, 2);
      end;
    jdtNull, jdtUnknown:
      if ANullConvert then
        ABuilder.Cat(CharStringStart).Cat(CharStringEnd)
      else
        ABuilder.Cat(CharNull, 5);
    jdtString:
      begin
      ABuilder.Cat(CharStringStart, 1);
      CatValue(ANode.FValue);
      ABuilder.Cat(CharStringEnd, 2);
      end;
    jdtInteger, jdtFloat, jdtBoolean:
      begin
      ABuilder.Cat(ANode.Value);
      ABuilder.Cat(CharComma, 1);
      end;
    jdtDateTime:
      begin
      ABuilder.Cat(CharStringStart, 1);
      if StrictJson then
        StrictJsonTime(ANode.AsDateTime)
      else
        ABuilder.Cat(ANode.Value);
      ABuilder.Cat(CharStringEnd, 1);
      ABuilder.Cat(CharComma, 1);
      end;
  end;
  end;

begin
Result := ABuilder;
DoEncode(Self, 0);
end;

procedure TQJson.InternalRttiFilter(ASender: TQJson; AObject: Pointer;
  APropName: QStringW; APropType: PTypeInfo; var Accept: Boolean;
  ATag: Pointer);
var
  ATagData: PQJsonInternalTagData;
  procedure DoNameFilter;
  var
    ps: PQCharW;
  begin
  if Length(ATagData.AcceptNames) > 0 then
    begin
    Accept := False;
    ps := StrIStrW(PQCharW(ATagData.AcceptNames), PQCharW(APropName));
    if (ps <> nil) and ((ps = PQCharW(ATagData.AcceptNames)) or (ps[-1] = ',')
      or (ps[-1] = ';')) then
      begin
      ps := ps + Length(APropName);
      Accept := (ps^ = ',') or (ps^ = ';') or (ps^ = #0);
      end;
    end
  else if Length(ATagData.IgnoreNames) > 0 then
    begin
    ps := StrIStrW(PQCharW(ATagData.IgnoreNames), PQCharW(APropName));
    Accept := true;
    if (ps <> nil) and ((ps = PQCharW(ATagData.IgnoreNames)) or (ps[-1] = ',')
      or (ps[-1] = ';')) then
      begin
      ps := ps + Length(APropName);
      Accept := not((ps^ = ',') or (ps^ = ';') or (ps^ = #0));
      end;
    end;
  end;

begin
ATagData := PQJsonInternalTagData(ATag);
if ATagData.TagType = ttNameFilter then
  begin
  DoNameFilter;
  Exit;
  end;
{$IFDEF UNICODE}
if ATagData.TagType = ttAnonEvent then
  begin
  ATagData.OnEvent(ASender, AObject, APropName, APropType, Accept,
    ATagData.Tag);
  end;
{$ENDIF UNICODE}
end;

function TQJson.IsChildOf(AParent: TQJson): Boolean;
begin
if Assigned(AParent) then
  begin
  if AParent = FParent then
    Result := true
  else
    Result := FParent.IsChildOf(AParent);
  end
else
  Result := False;
end;

function TQJson.IsParentOf(AChild: TQJson): Boolean;
begin
if Assigned(AChild) then
  Result := AChild.IsChildOf(Self)
else
  Result := False;
end;

function TQJson.ItemByName(AName: QStringW): TQJson;
var
  AChild: TQJson;
  I: Integer;
  ASelfName: String;
  function ArrayName: String;
  var
    ANamedItem: TQJson;
    AParentIndexes: String;
  begin
  ANamedItem := Self;
  while ANamedItem.Parent <> nil do
    begin
    if ANamedItem.Parent.IsArray then
      begin
      AParentIndexes := AParentIndexes + '[' +
        IntToStr(ANamedItem.ItemIndex) + ']';
      ANamedItem := ANamedItem.Parent;
      end
    else
      Break;
    end;
  Result := ANamedItem.Name + AParentIndexes;
  end;

begin
Result := nil;
if DataType = jdtObject then
  begin
  I := IndexOf(AName);
  if I <> -1 then
    Result := Items[I];
  end
else if DataType = jdtArray then
  begin
  ASelfName := ArrayName;
  for I := 0 to Count - 1 do
    begin
    AChild := Items[I];
    if ASelfName + '[' + IntToStr(I) + ']' = AName then
      begin
      Result := AChild;
      Exit;
      end
    else if AChild.IsArray then
      begin
      Result := AChild.ItemByName(AName);
      if Assigned(Result) then
        Exit;
      end
    else
    end;
  end;
end;

function TQJson.ItemByName(const AName: QStringW; AList: TQJsonItemList;
  ANest: Boolean): Integer;
var
  ANode: TQJson;
  function InternalFind(AParent: TQJson): Integer;
  var
    I: Integer;
  begin
  Result := 0;
  for I := 0 to AParent.Count - 1 do
    begin
    ANode := AParent.Items[I];
    if ANode.Name = AName then
      begin
      AList.Add(ANode);
      Inc(Result);
      end;
    if ANest then
      Inc(Result, InternalFind(ANode));
    end;
  end;

begin
Result := InternalFind(Self);
end;

function TQJson.ItemByPath(APath: QStringW): TQJson;
var
  AParent: TQJson;
  AName: QStringW;
  p, pn, ws: PQCharW;
  l: Integer;
  AIndex: Int64;
const
  PathDelimiters: PWideChar = './\';
begin
AParent := Self;
p := PQCharW(APath);
Result := nil;
while Assigned(AParent) and (p^ <> #0) do
  begin
  AName := DecodeTokenW(p, PathDelimiters, WideChar(0), False);
  if Length(AName) > 0 then
    begin
    // 查找的是数组？
    l := Length(AName);
    AIndex := -1;
    pn := PQCharW(AName);
    if (pn[l - 1] = ']') then
      begin
      repeat
        if pn[l] = '[' then
          begin
          ws := pn + l + 1;
          ParseInt(ws, AIndex);
          Break;
          end
        else
          Dec(l);
      until l = 0;
      if l > 0 then
        begin
        AName := StrDupX(pn, l);
        Result := AParent.ItemByName(AName);
        if Result <> nil then
          begin
          if Result.DataType <> jdtArray then
            Result := nil
          else if (AIndex >= 0) and (AIndex < Result.Count) then
            Result := Result[AIndex];
          end;
        end;
      end
    else
      Result := AParent.ItemByName(AName);
    if Assigned(Result) then
      AParent := Result
    else
      begin
      Exit;
      end;
    end;
  if CharInW(p, PathDelimiters) then
    Inc(p);
  // 否则是..或//\\等路径，忽略
  end;
if p^ <> #0 then
  Result := nil;
end;

function TQJson.ItemByRegex(const ARegex: QStringW; AList: TQJsonItemList;
  ANest: Boolean): Integer;
var
  ANode: TQJson;
  APcre: TPerlRegEx;
  function InternalFind(AParent: TQJson): Integer;
  var
    I: Integer;
  begin
  Result := 0;
  for I := 0 to AParent.Count - 1 do
    begin
    ANode := AParent.Items[I];
    APcre.Subject := ANode.Name;
    if APcre.Match then
      begin
      AList.Add(ANode);
      Inc(Result);
      end;
    if ANest then
      Inc(Result, InternalFind(ANode));
    end;
  end;

begin
APcre := TPerlRegEx.Create;
try
  APcre.RegEx := ARegex;
  APcre.Compile;
  Result := InternalFind(Self);
finally
  FreeObject(APcre);
end;
end;

procedure TQJson.LoadFromFile(AFileName: String; AEncoding: TTextEncoding);
var
  AStream: TFileStream;
begin
AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
try
  LoadFromStream(AStream, AEncoding);
finally
  FreeObject(AStream);
end;
end;

procedure TQJson.LoadFromStream(AStream: TStream; AEncoding: TTextEncoding);
var
  s: QStringW;
begin
s := LoadTextW(AStream, AEncoding);
if Length(s) > 2 then
  Parse(PQCharW(s), Length(s))
else
  raise Exception.Create(SBadJson);
end;

procedure TQJson.Parse(p: PWideChar; l: Integer);
  procedure ParseCopy;
  var
    s: QStringW;
  begin
  s := StrDupW(p, 0, l);
  p := PQCharW(s);
  ParseObject(p);
  end;

begin
if DataType in [jdtObject, jdtArray] then
  Clear;
if (l > 0) and (p[l] <> #0) then
  ParseCopy
else
  ParseObject(p);
end;

procedure TQJson.Parse(const s: QStringW);
begin
Parse(PQCharW(s), Length(s));
end;

procedure TQJson.ParseBlock(AStream: TStream; AEncoding: TTextEncoding);
var
  AMS: TMemoryStream;
  procedure ParseUCS2;
  var
    c: QCharW;
    ABlockCount: Integer;
  begin
  ABlockCount := 0;
  repeat
    if ABlockCount = 0 then
      begin
      repeat
        AStream.ReadBuffer(c, SizeOf(QCharW));
        AMS.WriteBuffer(c, SizeOf(QCharW));
      until c = '{';
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c, SizeOf(QCharW));
    if c = '{' then
      Inc(ABlockCount)
    else if c = '}' then
      Dec(ABlockCount);
    AMS.WriteBuffer(c, SizeOf(QCharW));
  until ABlockCount = 0;
  c := #0;
  AMS.Write(c, SizeOf(QCharW));
  Parse(AMS.Memory, AMS.Size - 1);
  end;

  procedure ParseUCS2BE;
  var
    c: Word;
    ABlockCount: Integer;
    p: PQCharW;
  begin
  ABlockCount := 0;
  repeat
    if ABlockCount = 0 then
      begin
      repeat
        AStream.ReadBuffer(c, SizeOf(Word));
        c := (c shr 8) or ((c shl 8) and $FF00);
        AMS.WriteBuffer(c, SizeOf(Word));
      until c = $7B; // #$7B={
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c, SizeOf(Word));
    c := (c shr 8) or ((c shl 8) and $FF00);
    if c = $7B then
      Inc(ABlockCount)
    else if c = $7D then // #$7D=}
      Dec(ABlockCount);
    AMS.WriteBuffer(c, SizeOf(QCharW));
  until ABlockCount = 0;
  c := 0;
  AMS.Write(c, SizeOf(QCharW));
  p := AMS.Memory;
  ParseObject(p);
  end;

  procedure ParseByByte;
  var
    c: Byte;
    ABlockCount: Integer;
  begin
  ABlockCount := 0;
  repeat
    if ABlockCount = 0 then
      begin
      repeat
        AStream.ReadBuffer(c, SizeOf(Byte));
        AMS.WriteBuffer(c, SizeOf(Byte));
      until c = $7B; // #$7B={
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c, SizeOf(Byte));
    if c = $7B then
      Inc(ABlockCount)
    else if c = $7D then // #$7D=}
      Dec(ABlockCount);
    AMS.WriteBuffer(c, SizeOf(Byte));
  until ABlockCount = 0;
  end;

  procedure ParseUtf8;
  var
    s: QStringW;
    p: PQCharW;
  begin
  ParseByByte;
  s := qstring.Utf8Decode(AMS.Memory, AMS.Size);
  p := PQCharW(s);
  ParseObject(p);
  end;

  procedure ParseAnsi;
  var
    s: QStringW;
  begin
  ParseByByte;
  s := qstring.AnsiDecode(AMS.Memory, AMS.Size);
  Parse(PQCharW(s));
  end;

begin
AMS := TMemoryStream.Create;
try
  if AEncoding = teAnsi then
    ParseAnsi
  else if AEncoding = teUtf8 then
    ParseUtf8
  else if AEncoding = teUnicode16LE then
    ParseUCS2
  else if AEncoding = teUnicode16BE then
    ParseUCS2BE
  else
    raise Exception.Create(SBadJsonEncoding);
finally
  AMS.Free;
end;
end;

function TQJson.ParseJsonPair(ABuilder: TQStringCatHelperW;
  var p: PQCharW): Integer;
const
  SpaceWithSemicolon: PWideChar = ': '#9#10#13#$3000;
  CommaWithSpace: PWideChar = ', '#9#10#13#$3000;
  JsonEndChars: PWideChar = ',}]';
  JsonComplexEnd: PWideChar = '}]';
var
  AChild: TQJson;
  AObjEnd: QCharW;
begin
// 遇到注释，则直接跳过
Result := 0;
while p^ = '/' do
  begin
  if StrictJson then
    begin
    Result := EParse_CommentNotSupport;
    Exit;
    end;
  if p[1] = '/' then
    begin
    SkipUntilW(p, [WideChar(10)]);
    SkipSpaceW(p);
    end
  else if p[1] = '*' then
    begin
    Inc(p, 2);
    while p^ <> #0 do
      begin
      if (p[0] = '*') and (p[1] = '/') then
        begin
        Inc(p, 2);
        SkipSpaceW(p);
        Break;
        end
      else
        Inc(p);
      end;
    end
  else
    begin
    Result := EParse_UnknownToken;
    Exit;
    end;
  end;
// 解析值
if (p^ = '{') or (p^ = '[') then // 对象
  begin
  try
    if p^ = '{' then
      begin
      DataType := jdtObject;
      AObjEnd := '}';
      end
    else
      begin
      DataType := jdtArray;
      AObjEnd := ']';
      end;
    Inc(p);
    SkipSpaceW(p);
    while (p^ <> #0) and (p^ <> AObjEnd) do
      begin
      AChild := Add;
      Result := AChild.ParseJsonPair(ABuilder, p);
      if Result <> 0 then
        Exit;
      if p^ = ',' then
        begin
        Inc(p);
        SkipSpaceW(p);
        end;
      end;
    if p^ <> AObjEnd then
      begin
      Result := EParse_BadJson;
      Exit;
      end
    else
      begin
      Inc(p);
      SkipSpaceW(p);
      end;
  except
    Clear;
    raise;
  end;
  end
else if Parent <> nil then
  begin
  if (Parent.DataType = jdtObject) and (Length(FName) = 0) then
    begin
    Result := ParseName(ABuilder, p);
    if Result <> 0 then
      Exit;
    end;
  Result := TryParseValue(ABuilder, p);
  if Result = 0 then
    begin
    if not CharInW(p, JsonEndChars) then
      Result := EParse_EndCharNeeded;
    end;
  end
else
  Result := EParse_BadJson;
end;

function TQJson.ParseJsonTime(p: PQCharW; var ATime: TDateTime): Boolean;
var
  MS, TimeZone: Int64;
begin
// Javascript日期格式为/DATE(自1970.1.1起到现在的毫秒数+时区)/
Result := False;
if not StartWithW(p, '/DATE', False) then
  Exit;
Inc(p, 5);
SkipSpaceW(p);
if p^ <> '(' then
  Exit;
Inc(p);
SkipSpaceW(p);
if ParseInt(p, MS) = 0 then
  Exit;
SkipSpaceW(p);
if (p^ = '+') or (p^ = '-') then
  begin
  if ParseInt(p, TimeZone) = 0 then
    Exit;
  SkipSpaceW(p);
  end
else
  TimeZone := 0;
if p^ = ')' then
  begin
  ATime := (MS div 86400000) + ((MS mod 86400000) / 86400000.0);
  if TimeZone <> 0 then
    ATime := IncHour(ATime, -TimeZone);
  Inc(p);
  SkipSpaceW(p);
  Result := true
  end;
end;

function TQJson.ParseName(ABuilder: TQStringCatHelperW; var p: PQCharW)
  : Integer;
begin
if StrictJson and (p^ <> '"') then
  begin
  Result := EParse_BadNameStart;
  Exit;
  end;
BuildJsonString(ABuilder, p);
if p^ <> ':' then
  begin
  Result := EParse_BadNameEnd;
  Exit;
  end;
if ABuilder.Position = 0 then
  begin
  Result := EParse_NameNotFound;
  Exit;
  end;
FName := ABuilder.Value;
// 解析名称完成
Inc(p);
SkipSpaceW(p);
Result := 0;
end;

procedure TQJson.ParseObject(var p: PQCharW);
var
  ABuilder: TQStringCatHelperW;
  ps: PQCharW;
  AErrorCode: Integer;
begin
ABuilder := TQStringCatHelperW.Create;
try
  ps := p;
  try
    SkipSpaceW(p);
    AErrorCode := ParseJsonPair(ABuilder, p);
    if AErrorCode <> 0 then
      RaiseParseException(AErrorCode, ps, p);
  except
    on E: Exception do
      begin
      if E is EJsonError then
        raise
      else
        raise Exception.Create(Self.FormatParseError(EParse_Unknown,
          E.Message, ps, p));
      end;
  end;
finally
  FreeObject(ABuilder);
end;
end;

procedure TQJson.ParseValue(ABuilder: TQStringCatHelperW; var p: PQCharW);
var
  ps: PQCharW;
begin
ps := p;
RaiseParseException(TryParseValue(ABuilder, p), ps, p);
end;

procedure TQJson.RaiseParseException(ACode: Integer; ps, p: PQCharW);
begin
if ACode <> 0 then
  begin
  case ACode of
    EParse_BadStringStart:
      raise EJsonError.Create(FormatParseError(ACode, SBadStringStart, ps, p));
    EParse_BadJson:
      raise EJsonError.Create(FormatParseError(ACode, SBadJson, ps, p));
    EParse_CommentNotSupport:
      raise EJsonError.Create(FormatParseError(ACode,
        SCommentNotSupport, ps, p));
    EParse_UnknownToken:
      raise EJsonError.Create(FormatParseError(ACode,
        SCommentNotSupport, ps, p));
    EParse_EndCharNeeded:
      raise EJsonError.Create(FormatParseError(ACode, SEndCharNeeded, ps, p));
    EParse_BadNameStart:
      raise EJsonError.Create(FormatParseError(ACode, SBadNameStart, ps, p));
    EParse_BadNameEnd:
      raise EJsonError.Create(FormatParseError(ACode, SBadNameEnd, ps, p));
    EParse_NameNotFound:
      raise EJsonError.Create(FormatParseError(ACode, SNameNotFound, ps, p))
  else
    raise EJsonError.Create(FormatParseError(ACode, SUnknownError, ps, p));
  end;
  end;
end;

procedure TQJson.Replace(AIndex: Integer; ANewItem: TQJson);
begin
FreeObject(Items[AIndex]);
FItems[AIndex] := ANewItem;
end;

procedure TQJson.ResetNull;
begin
DataType := jdtNull;
end;

procedure TQJson.SaveToFile(AFileName: String; AEncoding: TTextEncoding;
  AWriteBOM: Boolean);
var
  AStream: TMemoryStream;
begin
AStream := TMemoryStream.Create;
try
  SaveToStream(AStream, AEncoding, AWriteBOM);
  AStream.SaveToFile(AFileName);
finally
  FreeObject(AStream);
end;
end;

procedure TQJson.SaveToStream(AStream: TStream; AEncoding: TTextEncoding;
  AWriteBOM: Boolean);
begin
if AEncoding = teUtf8 then
  SaveTextU(AStream, qstring.Utf8Encode(Value), AWriteBOM)
else if AEncoding = teAnsi then
  SaveTextA(AStream, qstring.AnsiEncode(Value))
else if AEncoding = teUnicode16LE then
  SaveTextW(AStream, Value, AWriteBOM)
else
  SaveTextWBE(AStream, Value, AWriteBOM);
end;

procedure TQJson.SetAsArray(const Value: QStringW);
var
  p: PQCharW;
begin
DataType := jdtArray;
Clear;
p := PQCharW(Value);
ParseObject(p);
end;

procedure TQJson.SetAsBoolean(const Value: Boolean);
begin
DataType := jdtBoolean;
PBoolean(FValue)^ := Value;
end;

procedure TQJson.SetAsDateTime(const Value: TDateTime);
begin
DataType := jdtDateTime;
PExtended(FValue)^ := Value;
end;

procedure TQJson.SetAsFloat(const Value: Extended);
begin
if IsNan(Value) or IsInfinite(Value) then
  raise Exception.Create(SSupportFloat);
DataType := jdtFloat;
PExtended(FValue)^ := Value;
end;

procedure TQJson.SetAsInt64(const Value: Int64);
begin
DataType := jdtInteger;
PInt64(FValue)^ := Value;
end;

procedure TQJson.SetAsInteger(const Value: Integer);
begin
SetAsInt64(Value);
end;

procedure TQJson.SetAsJson(const Value: QStringW);
var
  ABuilder: TQStringCatHelperW;
  p: PQCharW;
begin
ABuilder := TQStringCatHelperW.Create;
try
  try
    if DataType in [jdtArray, jdtObject] then
      Clear;
    p := PQCharW(Value);
    ParseValue(ABuilder, p);
  except
    AsString := Value;
  end;
finally
  FreeObject(ABuilder);
end;
end;

procedure TQJson.SetAsObject(const Value: QStringW);
begin
Parse(PQCharW(Value), Length(Value));
end;

procedure TQJson.SetAsString(const Value: QStringW);
begin
DataType := jdtString;
FValue := Value;
end;

procedure TQJson.SetAsVariant(const Value: Variant);
var
  I: Integer;
begin
if VarIsArray(Value) then
  begin
  ArrayNeeded(jdtArray);
  Clear;
  for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
    to VarArrayHighBound(Value, VarArrayDimCount(Value)) do
    Add.AsVariant := Value[I];
  end
else
  begin
  case VarType(Value) of
    varEmpty, varNull, varUnknown:
      ResetNull;
    varSmallInt, varInteger, varByte, varShortInt, varWord,
      varLongWord, varInt64:
      AsInt64 := Value;
    varSingle, varDouble, varCurrency:
      AsFloat := Value;
    varDate:
      AsDateTime := Value;
    varOleStr, varString{$IFDEF UNICODE}, varUString{$ENDIF}:
      AsString := Value;
    varBoolean:
      AsBoolean := Value;
{$IF RtlVersion>=26}
    varUInt64:
      AsInt64 := Value;
    varRecord:
      FromRtti(PVarRecord(@Value).RecInfo, PVarRecord(@Value).PRecord);
{$IFEND >=XE5}
  end;
  end;
end;

procedure TQJson.SetDataType(const Value: TQJsonDataType);
begin
if FDataType <> Value then
  begin
  if DataType in [jdtArray, jdtObject] then
    begin
    Clear;
    if not(Value in [jdtArray, jdtObject]) then
      begin
      FreeObject(FItems);
      end;
    end;
  case Value of
    jdtNull, jdtUnknown, jdtString:
      SetLength(FValue, 0);
    jdtInteger:
      begin
      SetLength(FValue, SizeOf(Int64) shr 1);
      PInt64(FValue)^ := 0;
      end;
    jdtFloat, jdtDateTime:
      begin
      SetLength(FValue, SizeOf(Extended) shr 1);
      PDouble(FValue)^ := 0;
      end;
    jdtBoolean:
      begin
      SetLength(FValue, 1);
      PBoolean(FValue)^ := False;
      end;
    jdtArray, jdtObject:
      if not(FDataType in [jdtArray, jdtObject]) then
        ArrayNeeded(Value);
  end;
  FDataType := Value;
  end;
end;

procedure TQJson.SetValue(const Value: QStringW);
var
  p: PQCharW;
  procedure ParseNum;
  var
    ANum: Extended;
  begin
  if ParseNumeric(p, ANum) then
    begin
    if SameValue(ANum, Trunc(ANum)) then
      AsInt64 := Trunc(ANum)
    else
      AsFloat := ANum;
    end
  else
    raise Exception.Create(Format(SBadNumeric, [Value]));
  end;
  procedure SetDateTime;
  var
    ATime: TDateTime;
  begin
  if ParseDateTime(PQCharW(Value), ATime) then
    AsDateTime := ATime
  else if ParseJsonTime(PQCharW(Value), ATime) then
    AsDateTime := ATime
  else
    raise Exception.Create(SBadJsonTime);
  end;
  procedure DetectValue;
  var
    ABuilder: TQStringCatHelperW;
    p: PQCharW;
  begin
  ABuilder := TQStringCatHelperW.Create;
  try
    p := PQCharW(Value);
    ParseValue(ABuilder, p);
  except
    AsString := Value;
  end;
  FreeObject(ABuilder);
  end;

begin
if DataType = jdtString then
  FValue := Value
else if DataType = jdtBoolean then
  AsBoolean := StrToBool(Value)
else
  begin
  p := PQCharW(Value);
  if DataType in [jdtInteger, jdtFloat] then
    ParseNum
  else if DataType = jdtDateTime then
    SetDateTime
  else if DataType in [jdtArray, jdtObject] then
    begin
    Clear;
    ParseObject(p);
    end
  else // jdtUnknown
    DetectValue;
  end;
end;

{$IFDEF UNICODE}

function TQJson.Invoke(AInstance: TValue): TValue;
var
  AMethods: TArray<TRttiMethod>;
  AParams: TArray<TRttiParameter>;
  AMethod: TRttiMethod;
  AType: TRttiType;
  AContext: TRttiContext;
  AParamValues: array of TValue;
  I, c: Integer;
  AParamItem: TQJson;
begin
AContext := TRttiContext.Create;
Result := TValue.Empty;
if AInstance.IsObject then
  AType := AContext.GetType(AInstance.AsObject.ClassInfo)
else if AInstance.IsClass then
  AType := AContext.GetType(AInstance.AsClass)
else if AInstance.Kind = tkRecord then
  AType := AContext.GetType(AInstance.TypeInfo)
else
  AType := AContext.GetType(AInstance.TypeInfo);
AMethods := AType.GetMethods(Name);
c := Count;
for AMethod in AMethods do
  begin
  AParams := AMethod.GetParameters;
  if Length(AParams) = c then
    begin
    SetLength(AParamValues, c);
    for I := 0 to c - 1 do
      begin
      AParamItem := ItemByName(AParams[I].Name);
      if AParamItem <> nil then
        AParamValues[I] := AParamItem.ToRttiValue
      else
        raise Exception.CreateFmt(SParamMissed, [AParams[I].Name]);
      end;
    Result := AMethod.Invoke(AInstance, AParamValues);
    Exit;
    end;
  end;
raise Exception.CreateFmt(SMethodMissed, [Name]);
end;

procedure TQJson.ToRecord<T>(var ARecord: T);
begin
ToRtti(@ARecord, TypeInfo(T));
end;

procedure TQJson.ToRtti(AInstance: TValue);
begin
if AInstance.IsEmpty then
  Exit;
if AInstance.Kind = tkRecord then
  ToRtti(AInstance.GetReferenceToRawData, AInstance.TypeInfo)
else if AInstance.Kind = tkClass then
  ToRtti(AInstance.AsObject, AInstance.TypeInfo)
end;

procedure TQJson.ToRtti(ADest: Pointer; AType: PTypeInfo);

  procedure LoadCollection(AJson: TQJson; ACollection: TCollection);
  var
    I: Integer;
  begin
  for I := 0 to AJson.Count - 1 do
    AJson.ToRtti(ACollection.Add);
  end;
  procedure ToRecord;
  var
    AContext: TRttiContext;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    ABaseAddr: Pointer;
    J: Integer;
    AChild: TQJson;
    AObj: TObject;
  begin
  AContext := TRttiContext.Create;
  ARttiType := AContext.GetType(AType);
  ABaseAddr := ADest;
  AFields := ARttiType.GetFields;
  for J := Low(AFields) to High(AFields) do
    begin
    if AFields[J].FieldType <> nil then
      begin
      AChild := ItemByName(AFields[J].Name);
      if AChild <> nil then
        begin
        case AFields[J].FieldType.TypeKind of
          tkInteger:
            AFields[J].SetValue(ABaseAddr, AChild.AsInteger);
{$IFNDEF NEXTGEN}
          tkString:
            PShortString(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
              ShortString(AChild.AsString);
{$ENDIF !NEXTGEN}
          tkUString{$IFNDEF NEXTGEN}, tkLString, tkWString{$ENDIF !NEXTGEN}:
            AFields[J].SetValue(ABaseAddr, AChild.AsString);
          tkEnumeration:
            begin
            if GetTypeData(AFields[J].FieldType.Handle)^.BaseType^ = TypeInfo
              (Boolean) then
              AFields[J].SetValue(ABaseAddr, AChild.AsBoolean)
            else
              begin
              case GetTypeData(AFields[J].FieldType.Handle).OrdType of
                otSByte:
                  begin
                  if AChild.DataType = jdtInteger then
                    PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otUByte:
                  begin
                  if AChild.DataType = jdtInteger then
                    PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otSWord:
                  begin
                  if AChild.DataType = jdtInteger then
                    PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otUWord:
                  begin
                  if AChild.DataType = jdtInteger then
                    PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otSLong:
                  begin
                  if AChild.DataType = jdtInteger then
                    PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otULong:
                  begin
                  if AChild.DataType = jdtInteger then
                    PCardinal(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PCardinal(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
              end;
              end;
            end;
          tkSet:
            begin
            case GetTypeData(AFields[J].FieldType.Handle).OrdType of
              otSByte:
                begin
                if AChild.DataType = jdtInteger then
                  PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otUByte:
                begin
                if AChild.DataType = jdtInteger then
                  PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otSWord:
                begin
                if AChild.DataType = jdtInteger then
                  PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otUWord:
                begin
                if AChild.DataType = jdtInteger then
                  PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otSLong:
                begin
                if AChild.DataType = jdtInteger then
                  PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otULong:
                begin
                if AChild.DataType = jdtInteger then
                  PCardinal(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PCardinal(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
            end;
            end;
          tkChar, tkWChar:
            AFields[J].SetValue(ABaseAddr, AChild.AsString);
          tkFloat:
            if (AFields[J].FieldType.Handle = TypeInfo(TDateTime)) or
              (AFields[J].FieldType.Handle = TypeInfo(TTime)) or
              (AFields[J].FieldType.Handle = TypeInfo(TDate)) then
              begin
              if AChild.IsDateTime then
                AFields[J].SetValue(ABaseAddr, AChild.AsDateTime)
              else if AChild.DataType in [jdtNull, jdtUnknown] then
                AFields[J].SetValue(ABaseAddr, 0)
              else
                raise Exception.CreateFmt(SBadConvert,
                  [AChild.AsString, JsonTypeName[AChild.DataType]]);
              end
            else
              AFields[J].SetValue(ABaseAddr, AChild.AsFloat);
          tkInt64:
            AFields[J].SetValue(ABaseAddr, AChild.AsInt64);
          tkVariant:
            PVariant(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
              AChild.AsVariant;
          tkArray, tkDynArray:
            AChild.ToRtti(Pointer(IntPtr(ABaseAddr) + AFields[J].Offset),
              AFields[J].FieldType.Handle);
          tkClass:
            begin
            AObj := AFields[J].GetValue(ABaseAddr).AsObject;
            if AObj is TStrings then
              (AObj as TStrings).Text := AChild.AsString
            else if AObj is TCollection then
              LoadCollection(AChild, AObj as TCollection)
            else
              AChild.ToRtti(AObj);
            end;
          tkRecord:
            AChild.ToRtti(Pointer(IntPtr(ABaseAddr) + AFields[J].Offset),
              AFields[J].FieldType.Handle);
        end;
        end;
      end;
    end;
  end;

  procedure ToObject;
  var
    AProp: PPropInfo;
    ACount: Integer;
    J: Integer;
    AObj, AChildObj: TObject;
    AChild: TQJson;
  begin
  AObj := ADest;
  ACount := Count;
  for J := 0 to ACount - 1 do
    begin
    AChild := Items[J];
    AProp := GetPropInfo(AObj, AChild.Name);
    if AProp <> nil then
      begin
      case AProp.PropType^.Kind of
        tkClass:
          begin
          AChildObj := Pointer(GetOrdProp(AObj, AProp));
          if AChildObj is TStrings then
            (AChildObj as TStrings).Text := AChild.AsString
          else if AChildObj is TCollection then
            LoadCollection(AChild, AChildObj as TCollection)
          else
            AChild.ToRtti(AChildObj);
          end;
        tkRecord, tkArray, tkDynArray: // tkArray,tkDynArray类型的属性没见过,tkRecord存疑
          begin
          AChild.ToRtti(Pointer(GetOrdProp(AObj, AProp)), AProp.PropType^);
          end;
        tkInteger:
          SetOrdProp(AObj, AProp, AChild.AsInteger);
        tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
          SetStrProp(AObj, AProp, AChild.AsString);
        tkEnumeration:
          begin
          if GetTypeData(AProp.PropType^)^.BaseType^ = TypeInfo(Boolean) then
            SetOrdProp(AObj, AProp, Integer(AChild.AsBoolean))
          else if AChild.DataType = jdtInteger then
            SetOrdProp(AObj, AProp, AChild.AsInteger)
          else
            SetEnumProp(AObj, AProp, AChild.AsString);
          end;
        tkSet:
          begin
          if AChild.DataType = jdtInteger then
            SetOrdProp(AObj, AProp, AChild.AsInteger)
          else
            SetSetProp(AObj, AProp, AChild.AsString);
          end;
        tkVariant:
          SetVariantProp(AObj, AProp, AChild.AsVariant);
        tkInt64:
          SetInt64Prop(AObj, AProp, AChild.AsInt64);
      end;
      end;
    end;
  end;

  procedure SetDynArrayLen(arr: Pointer; AType: PTypeInfo; ALen: NativeInt);
  var
    pmem: Pointer;
  begin
  pmem := PPointer(arr)^;
  DynArraySetLength(pmem, AType, 1, @ALen);
  PPointer(arr)^ := pmem;
  end;

  procedure ToArray;
  var
    AContext: TRttiContext;
    ASubType: TRttiType;
    I, l, AOffset: Integer;
    s: QStringW;
    pd, pi: PByte;
    AChildObj: TObject;
    ASubTypeInfo: PTypeInfo;
    AChild: TQJson;
  begin
  AContext := TRttiContext.Create;
{$IF RTLVersion>25}
  s := ArrayItemTypeName(AType.NameFld.ToString);
{$ELSE}
  s := ArrayItemTypeName(String(AType.Name));
{$IFEND}
  ASubType := AContext.FindType(s);
  ASubTypeInfo := ASubType.Handle;
  if ASubType <> nil then
    begin
    l := Count;
    SetDynArrayLen(ADest, AType, l);
    pd := PPointer(ADest)^;
    for I := 0 to l - 1 do
      begin
      AOffset := I * GetTypeData(AType).elSize;
      pi := Pointer(IntPtr(pd) + AOffset);
      AChild := Items[I];
      case ASubType.TypeKind of
        tkInteger:
          begin
          case GetTypeData(ASubTypeInfo).OrdType of
            otSByte:
              PShortint(pi)^ := AChild.AsInteger;
            otUByte:
              pi^ := Items[I].AsInteger;
            otSWord:
              PSmallint(pi)^ := AChild.AsInteger;
            otUWord:
              PWord(pi)^ := AChild.AsInteger;
            otSLong:
              PInteger(pi)^ := AChild.AsInteger;
            otULong:
              PCardinal(pi)^ := AChild.AsInteger;
          end;
          end;
{$IFNDEF NEXTGEN}
        tkChar:
          pi^ := Ord(PAnsiChar(AnsiString(AChild.AsString))[0]);
{$ENDIF !NEXTGEN}
        tkEnumeration:
          begin
          if GetTypeData(ASubTypeInfo)^.BaseType^ = TypeInfo(Boolean) then
            PBoolean(pi)^ := AChild.AsBoolean
          else
            begin
            case GetTypeData(ASubTypeInfo)^.OrdType of
              otSByte:
                begin
                if AChild.DataType = jdtInteger then
                  PShortint(pi)^ := AChild.AsInteger
                else
                  PShortint(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                end;
              otUByte:
                begin
                if AChild.DataType = jdtInteger then
                  pi^ := AChild.AsInteger
                else
                  pi^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                end;
              otSWord:
                begin
                if AChild.DataType = jdtInteger then
                  PSmallint(pi)^ := AChild.AsInteger
                else
                  PSmallint(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                end;
              otUWord:
                begin
                if AChild.DataType = jdtInteger then
                  PWord(pi)^ := AChild.AsInteger
                else
                  PWord(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                end;
              otSLong:
                begin
                if AChild.DataType = jdtInteger then
                  PInteger(pi)^ := AChild.AsInteger
                else
                  PInteger(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                end;
              otULong:
                begin
                if AChild.DataType = jdtInteger then
                  PCardinal(pi)^ := AChild.AsInteger
                else
                  PCardinal(pi)^ := GetEnumValue(ASubTypeInfo,
                    Items[I].AsString);
                end;
            end;
            end;
          end;
        tkFloat:
          case GetTypeData(ASubTypeInfo)^.FloatType of
            ftSingle:
              PSingle(pi)^ := Items[I].AsFloat;
            ftDouble:
              PDouble(pi)^ := Items[I].AsFloat;
            ftExtended:
              PExtended(pi)^ := Items[I].AsFloat;
            ftComp:
              PComp(pi)^ := Items[I].AsFloat;
            ftCurr:
              PCurrency(pi)^ := Items[I].AsFloat;
          end;
{$IFNDEF NEXTGEN}
        tkString:
          PShortString(pi)^ := ShortString(Items[I].AsString);
{$ENDIF !NEXTGEN}
        tkSet:
          begin
          case GetTypeData(ASubTypeInfo)^.OrdType of
            otSByte:
              begin
              if AChild.DataType = jdtInteger then
                PShortint(pi)^ := AChild.AsInteger
              else
                PShortint(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
              end;
            otUByte:
              begin
              if AChild.DataType = jdtInteger then
                pi^ := AChild.AsInteger
              else
                pi^ := StringToSet(ASubTypeInfo, AChild.AsString);
              end;
            otSWord:
              begin
              if AChild.DataType = jdtInteger then
                PSmallint(pi)^ := AChild.AsInteger
              else
                PSmallint(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
              end;
            otUWord:
              begin
              if AChild.DataType = jdtInteger then
                PWord(pi)^ := AChild.AsInteger
              else
                PWord(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
              end;
            otSLong:
              begin
              if AChild.DataType = jdtInteger then
                PInteger(pi)^ := AChild.AsInteger
              else
                PInteger(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
              end;
            otULong:
              begin
              if AChild.DataType = jdtInteger then
                PCardinal(pi)^ := AChild.AsInteger
              else
                PCardinal(pi)^ := StringToSet(ASubTypeInfo, Items[I].AsString);
              end;
          end;
          end;
        tkClass:
          begin
          if PPointer(pi)^ <> nil then
            begin
            AChildObj := PPointer(pi)^;
            if AChildObj is TStrings then
              (AChildObj as TStrings).Text := Items[I].AsString
            else if AChildObj is TCollection then
              LoadCollection(Items[I], AChildObj as TCollection)
            else
              Items[I].ToRtti(AChildObj);
            end;
          end;
        tkWChar:
          PWideChar(pi)^ := PWideChar(Items[I].AsString)[0];
{$IFNDEF NEXTGEN}
        tkLString:
          PAnsiString(pi)^ := AnsiString(Items[I].AsString);
        tkWString:
          PWideString(pi)^ := Items[I].AsString;
{$ENDIF}
        tkVariant:
          PVariant(pi)^ := Items[I].AsVariant;
        tkArray, tkDynArray:
          Items[I].ToRtti(pi, ASubTypeInfo);
        tkRecord:
          Items[I].ToRtti(pi, ASubTypeInfo);
        tkInt64:
          PInt64(pi)^ := Items[I].AsInt64;
        tkUString:
          PUnicodeString(pi)^ := Items[I].AsString;
      end;
      end;
    end
  else
    raise Exception.Create(SArrayTypeMissed);
  end;
  function GetFixedArrayItemType: PTypeInfo;
  var
    pType: PPTypeInfo;
  begin
  pType := GetTypeData(AType)^.ArrayData.ElType;
  if pType = nil then
    Result := nil
  else
    Result := pType^;
  end;
  procedure ToFixedArray;
  var
    I, c, ASize: Integer;
    ASubType: PTypeInfo;
    AChild: TQJson;
    AChildObj: TObject;
    pi: Pointer;
  begin
  c := GetTypeData(AType).ArrayData.ElCount;
  ASubType := GetFixedArrayItemType;
  if ASubType = nil then
    Exit;
  ASize := GetTypeData(ASubType).elSize;
  for I := 0 to c - 1 do
    begin
    pi := Pointer(IntPtr(ADest) + ASize * I);
    AChild := Items[I];
    case ASubType.Kind of
      tkInteger:
        begin
        case GetTypeData(ASubType).OrdType of
          otSByte:
            PShortint(pi)^ := AChild.AsInteger;
          otUByte:
            PByte(pi)^ := AChild.AsInteger;
          otSWord:
            PSmallint(pi)^ := AChild.AsInteger;
          otUWord:
            PWord(pi)^ := AChild.AsInteger;
          otSLong:
            PInteger(pi)^ := AChild.AsInteger;
          otULong:
            PCardinal(pi)^ := AChild.AsInteger;
        end;
        end;
{$IFNDEF NEXTGEN}
      tkChar:
        PByte(pi)^ := Ord(PAnsiChar(AnsiString(AChild.AsString))[0]);
{$ENDIF !NEXTGEN}
      tkEnumeration:
        begin
        if GetTypeData(ASubType)^.BaseType^ = TypeInfo(Boolean) then
          PBoolean(pi)^ := AChild.AsBoolean
        else
          begin
          case GetTypeData(ASubType)^.OrdType of
            otSByte:
              begin
              if AChild.DataType = jdtInteger then
                PShortint(pi)^ := AChild.AsInteger
              else
                PShortint(pi)^ := GetEnumValue(ASubType, AChild.AsString);
              end;
            otUByte:
              begin
              if AChild.DataType = jdtInteger then
                PByte(pi)^ := AChild.AsInteger
              else
                PByte(pi)^ := GetEnumValue(ASubType, AChild.AsString);
              end;
            otSWord:
              begin
              if AChild.DataType = jdtInteger then
                PSmallint(pi)^ := AChild.AsInteger
              else
                PSmallint(pi)^ := GetEnumValue(ASubType, AChild.AsString);
              end;
            otUWord:
              begin
              if AChild.DataType = jdtInteger then
                PWord(pi)^ := AChild.AsInteger
              else
                PWord(pi)^ := GetEnumValue(ASubType, AChild.AsString);
              end;
            otSLong:
              begin
              if AChild.DataType = jdtInteger then
                PInteger(pi)^ := AChild.AsInteger
              else
                PInteger(pi)^ := GetEnumValue(ASubType, AChild.AsString);
              end;
            otULong:
              begin
              if AChild.DataType = jdtInteger then
                PCardinal(pi)^ := AChild.AsInteger
              else
                PCardinal(pi)^ := GetEnumValue(ASubType, Items[I].AsString);
              end;
          end;
          end;
        end;
      tkFloat:
        case GetTypeData(ASubType)^.FloatType of
          ftSingle:
            PSingle(pi)^ := Items[I].AsFloat;
          ftDouble:
            PDouble(pi)^ := Items[I].AsFloat;
          ftExtended:
            PExtended(pi)^ := Items[I].AsFloat;
          ftComp:
            PComp(pi)^ := Items[I].AsFloat;
          ftCurr:
            PCurrency(pi)^ := Items[I].AsFloat;
        end;
{$IFNDEF NEXTGEN}
      tkString:
        PShortString(pi)^ := ShortString(Items[I].AsString);
{$ENDIF !NEXTGEN}
      tkSet:
        begin
        case GetTypeData(ASubType)^.OrdType of
          otSByte:
            begin
            if AChild.DataType = jdtInteger then
              PShortint(pi)^ := AChild.AsInteger
            else
              PShortint(pi)^ := StringToSet(ASubType, AChild.AsString);
            end;
          otUByte:
            begin
            if AChild.DataType = jdtInteger then
              PByte(pi)^ := AChild.AsInteger
            else
              PByte(pi)^ := StringToSet(ASubType, AChild.AsString);
            end;
          otSWord:
            begin
            if AChild.DataType = jdtInteger then
              PSmallint(pi)^ := AChild.AsInteger
            else
              PSmallint(pi)^ := StringToSet(ASubType, AChild.AsString);
            end;
          otUWord:
            begin
            if AChild.DataType = jdtInteger then
              PWord(pi)^ := AChild.AsInteger
            else
              PWord(pi)^ := StringToSet(ASubType, AChild.AsString);
            end;
          otSLong:
            begin
            if AChild.DataType = jdtInteger then
              PInteger(pi)^ := AChild.AsInteger
            else
              PInteger(pi)^ := StringToSet(ASubType, AChild.AsString);
            end;
          otULong:
            begin
            if AChild.DataType = jdtInteger then
              PCardinal(pi)^ := AChild.AsInteger
            else
              PCardinal(pi)^ := StringToSet(ASubType, Items[I].AsString);
            end;
        end;
        end;
      tkClass:
        begin
        if PPointer(pi)^ <> nil then
          begin
          AChildObj := PPointer(pi)^;
          if AChildObj is TStrings then
            (AChildObj as TStrings).Text := Items[I].AsString
          else if AChildObj is TCollection then
            LoadCollection(Items[I], AChildObj as TCollection)
          else
            Items[I].ToRtti(AChildObj);
          end;
        end;
      tkWChar:
        PWideChar(pi)^ := PWideChar(Items[I].AsString)[0];
{$IFNDEF NEXTGEN}
      tkLString:
        PAnsiString(pi)^ := AnsiString(Items[I].AsString);
      tkWString:
        PWideString(pi)^ := Items[I].AsString;
{$ENDIF}
      tkVariant:
        PVariant(pi)^ := Items[I].AsVariant;
      tkArray, tkDynArray:
        Items[I].ToRtti(pi, ASubType);
      tkRecord:
        Items[I].ToRtti(pi, ASubType);
      tkInt64:
        PInt64(pi)^ := Items[I].AsInt64;
      tkUString:
        PUnicodeString(pi)^ := Items[I].AsString;
    end;
    end;
  end;

begin
if ADest <> nil then
  begin
  if AType.Kind = tkRecord then
    ToRecord
  else if AType.Kind = tkClass then
    ToObject
  else if AType.Kind = tkDynArray then
    ToArray
  else if AType.Kind = tkArray then
    ToFixedArray
  else
    raise Exception.Create(SUnsupportPropertyType);
  end;
end;

function TQJson.ToRttiValue: TValue;
  procedure AsDynValueArray;
  var
    AValues: array of TValue;
    I: Integer;
  begin
  SetLength(AValues, Count);
  for I := 0 to Count - 1 do
    AValues[I] := Items[I].ToRttiValue;
  Result := TValue.FromArray(TypeInfo(TValueArray), AValues);
  end;

begin
case DataType of
  jdtString:
    Result := AsString;
  jdtInteger:
    Result := AsInt64;
  jdtFloat:
    Result := AsFloat;
  jdtDateTime:
    Result := AsDateTime;
  jdtBoolean:
    Result := AsBoolean;
  jdtArray, jdtObject: // 数组和对象都只能当成数组来处理
    AsDynValueArray
else
  Result := TValue.Empty;
end;
end;
{$ENDIF >XE5}

function TQJson.ToString: string;
begin
Result := AsString;
end;

function TQJson.TryParse(p: PWideChar; l: Integer): Boolean;

  procedure DoTry;
  var
    ABuilder: TQStringCatHelperW;
  begin
  ABuilder := TQStringCatHelperW.Create;
  try
    try
      SkipSpaceW(p);
      Result := ParseJsonPair(ABuilder, p) = 0;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    FreeObject(ABuilder);
  end;
  end;

  procedure ParseCopy;
  var
    s: QStringW;
  begin
  s := StrDupW(p, 0, l);
  p := PQCharW(s);
  DoTry;
  end;

begin
if DataType in [jdtObject, jdtArray] then
  Clear;
if (l > 0) and (p[l] <> #0) then
  ParseCopy
else
  DoTry;
end;

function TQJson.TryParse(const s: QStringW): Boolean;
begin
Result := TryParse(PQCharW(s), Length(s));
end;

function TQJson.TryParseValue(ABuilder: TQStringCatHelperW;
  var p: PQCharW): Integer;
var
  ANum: Extended;
const
  JsonEndChars: PWideChar = ',]}';
begin
Result := 0;
if p^ = '"' then
  begin
  BuildJsonString(ABuilder, p);
  AsString := ABuilder.Value;
  end
else if p^ = '''' then
  begin
  if StrictJson then
    Result := EParse_BadStringStart;
  BuildJsonString(ABuilder, p);
  AsString := ABuilder.Value;
  end
else if ParseNumeric(p, ANum) then // 数字？
  begin
  SkipSpaceW(p);
  if (p^ = #0) or CharInW(p, JsonEndChars) then
    begin
    if SameValue(ANum, Trunc(ANum)) then
      AsInt64 := Trunc(ANum)
    else
      AsFloat := ANum;
    end
  else
    Result := EParse_BadJson;
  end
else if StartWithW(p, 'False', true) then // False
  begin
  Inc(p, 5);
  SkipSpaceW(p);
  AsBoolean := False
  end
else if StartWithW(p, 'True', true) then // True
  begin
  Inc(p, 4);
  SkipSpaceW(p);
  AsBoolean := true;
  end
else if StartWithW(p, 'NULL', true) then // Null
  begin
  Inc(p, 4);
  SkipSpaceW(p);
  ResetNull;
  end
else if (p^ = '[') or (p^ = '{') then
  Result := ParseJsonPair(ABuilder, p)
else
  Result := 2;
end;

procedure TQJson.ValidArray;
begin
if DataType in [jdtArray, jdtObject] then
{$IFDEF UNICODE}
  FItems := TList<TQJson>.Create
{$ELSE}
  FItems := TList.Create
{$ENDIF}
else
  raise Exception.Create(Format(SVarNotArray, [FName]));
end;

function TQJson.ValueByName(AName, ADefVal: QStringW): QStringW;
var
  AChild: TQJson;
begin
AChild := ItemByName(AName);
if Assigned(AChild) then
  Result := AChild.Value
else
  Result := ADefVal;
end;

function TQJson.ValueByPath(APath, ADefVal: QStringW): QStringW;
var
  AItem: TQJson;
begin
AItem := ItemByPath(APath);
if Assigned(AItem) then
  Result := AItem.Value
else
  Result := ADefVal;
end;
{ TQJsonEnumerator }

constructor TQJsonEnumerator.Create(AList: TQJson);
begin
inherited Create;
FList := AList;
FIndex := -1;
end;

function TQJsonEnumerator.GetCurrent: TQJson;
begin
Result := FList[FIndex];
end;

function TQJsonEnumerator.MoveNext: Boolean;
begin
if FIndex < FList.Count - 1 then
  begin
  Inc(FIndex);
  Result := true;
  end
else
  Result := False;
end;

{ TQHashedJson }

function TQHashedJson.Add(AName: QStringW): TQJson;
begin
Result := inherited Add(AName);
Result.FNameHash := HashOf(PQCharW(AName), Length(AName) shl 1);
FHashTable.Add(Pointer(Count - 1), Result.FNameHash);
end;

procedure TQHashedJson.Assign(ANode: TQJson);
begin
inherited;
if (Length(FName) > 0) then
  begin
  if FNameHash = 0 then
    FNameHash := HashOf(PQCharW(FName), Length(FName) shl 1);
  if Assigned(Parent) then
    TQHashedJson(Parent).FHashTable.Add(Pointer(Parent.Count - 1), FNameHash);
  end;
end;

procedure TQHashedJson.Clear;
begin
inherited;
FHashTable.Clear;
end;

constructor TQHashedJson.Create;
begin
inherited;
FHashTable := TQHashTable.Create();
FHashTable.AutoSize := true;
end;

function TQHashedJson.CreateJson: TQJson;
begin
if Assigned(OnQJsonCreate) then
  Result := OnQJsonCreate
else
  Result := TQHashedJson.Create;
end;

procedure TQHashedJson.Delete(AIndex: Integer);
var
  AItem: TQJson;
begin
AItem := Items[AIndex];
FHashTable.Delete(Pointer(AIndex), AItem.NameHash);
inherited;
end;

destructor TQHashedJson.Destroy;
begin
FreeObject(FHashTable);
inherited;
end;

function TQHashedJson.IndexOf(const AName: QStringW): Integer;
var
  AIndex, AHash: Integer;
  AList: PQHashList;
  AItem: TQJson;
begin
AHash := HashOf(PQCharW(AName), Length(AName) shl 1);
AList := FHashTable.FindFirst(AHash);
Result := -1;
while AList <> nil do
  begin
  AIndex := Integer(AList.Data);
  AItem := Items[AIndex];
  if AItem.Name = AName then
    begin
    Result := AIndex;
    Break;
    end
  else
    AList := FHashTable.FindNext(AList);
  end;
end;

procedure TQHashedJson.Replace(AIndex: Integer; ANewItem: TQJson);
var
  AOld: TQJson;
begin
if not(ANewItem is TQHashedJson) then
  raise Exception.CreateFmt(SReplaceTypeNeed, ['TQHashedJson']);
AOld := Items[AIndex];
FHashTable.Delete(Pointer(AIndex), AOld.NameHash);
inherited;
if Length(ANewItem.FName) > 0 then
  FHashTable.Add(Pointer(AIndex), ANewItem.FNameHash);
end;

initialization

StrictJson := False;
JsonRttiEnumAsInt := true;
JsonCaseSensitive := true;
JsonDateFormat := 'yyyy-mm-dd';
JsonDateTimeFormat := 'yyyy-mm-dd''T''hh:nn:ss.zzz';
JsonTimeFormat := 'hh:nn:ss.zzz';
OnQJsonCreate := nil;
OnQJsonFree := nil;

end.
