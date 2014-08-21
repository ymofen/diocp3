unit qmsgpack;
{$i 'qdac.inc'}

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
  ==========
  * 修正了加载长二进制数据时，错误的跳过内容的前4个字节的问题(天地弦报告)
  * 优化了AsVariant读写时，对字节数组时的转换效率
  * 修正了CopyValue时，对于非字符串类型拷贝长度设置错误的问题
  * 网络字节顺序和主机字节顺序转换统一改为使用ExchangeByteOrder函数完成

  2014.8.2
  =========
  * 修正了SetAsString时如果长度为0时，未检查数组FValue造成访问越界的问题

  2014.7.17
  =========
  * 合并QJson的相关RTTI函数(AK47完成)

  2014.7.16
  =========
  * 修正了GetPath时，未初始化结果字符串造成Path属性可能出错的问题
  2014.7.7
  =========
  * 修正了整数值-1~-31时，解码出错的问题(五毒报告)
  2014.7.3
  =========
  * 修正了Assign时复制了当前结点名称的问题
  2014.7.1
  =========
  * 修改AsString在空值时的返回内容为空字符串
  2014.6.28
  =========
  * 修正了ForcePath('Items[]')默认添加了空子结点的问题(pony,光明报告)
  + 加入MsgPackRttiEnumAsInt全局选项，控制枚举值和集合值是否保存成其字符串表达，默认为True(同步自QJson)
  2014.6.27
  =========
  * 修正了FromRTTI时，对于方法、事件等属性没有进行过滤的问题
  * 修正了ToRtti.ToArray时，对于动态数组的设置长度时类型错误
  * 修改了AsVariant的行为，对字节数组直接转换为TBytes，而不再使用普通的数组
  2014.6.26
  ==========
  * 修正了ToRtti.ToRecord子函数处理日期类型时的错误(感谢群友飞鸿大量的RTTI建议和测试)
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
  2014.6.23
  ==========
  + FromRecord支持动态数组和普通数组
  2014.6.21
  ==========
  + 增加RTTI函数支持(Invoke/FromRtti/ToRtti/FromRecord/ToRecord)
  2014.6.20
  ==========
  + 增加对Single类型的支持(AsSingle)，这样全部MessagePack格式的支持完整了
  2014.6.19
  ==========
  * 修正了QMsgPack解码时，对于长度为0的字符串解码出错的问题
  2014.6.17
  ==========
  * 首个正式版本发布，目前与RTTI相关的几个函数暂时不可用
}
uses classes, sysutils, math, qstring, qrbtree, typinfo,
  variants
{$IFDEF UNICODE}, Generics.Collections, Rtti{$ENDIF}
{$IF RTLVersion<22}// 2007-2010
    , PerlRegEx, pcre
{$ELSE}
    , RegularExpressionsCore
{$IFEND};
{$HPPEMIT '#pragma link "qmsgpack"'}

type
  TQMsgPack = class;
  TQMsgPackType = (mptUnknown, mptInteger, mptNull, mptBoolean, mptSingle,
    mptFloat, mptString, mptBinary, mptArray, mptMap, mptExtended, mptDateTime);
{$IFDEF UNICODE}
  /// <summary>
  /// RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQMsgPack对象</param>
  /// <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  /// <param name="AType">属性或字段的类型信息</param>
  /// <param name="Accept">是否记录该属性或字段</param>
  /// <param name="ATag">用户自定义的附加数据成员</param>
  TQMsgPackRttiFilterEventA = reference to procedure(ASender: TQMsgPack;
    AObject: Pointer; AName: QStringW; AType: PTypeInfo; var Accept: Boolean;
    ATag: Pointer);
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQMsgPack对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQMsgPackFilterEventA = reference to procedure(ASender, AItem: TQMsgPack;
    var Accept: Boolean; ATag: Pointer);
{$ENDIF UNICODE}
  /// <summary>
  /// RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQMsgPack对象</param>
  /// <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  /// <param name="AType">属性或字段的类型信息</param>
  /// <param name="Accept">是否记录该属性或字段</param>
  /// <param name="ATag">用户自定义的附加数据成员</param>
  TQMsgPackRttiFilterEvent = procedure(ASender: TQMsgPack; AObject: Pointer;
    AName: QStringW; AType: PTypeInfo; var Accept: Boolean; ATag: Pointer)
    of object;
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQMsgPack对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQMsgPackFilterEvent = procedure(ASender, AItem: TQMsgPack;
    var Accept: Boolean; ATag: Pointer) of object;
{$IFDEF UNICODE}
  TQMsgPackList = TList<TQMsgPack>;
{$ELSE}
  TQMsgPackList = TList;
{$ENDIF}

  TQMsgPackEnumerator = class
  private
    FIndex: Integer;
    FList: TQMsgPack;
  public
    constructor Create(AList: TQMsgPack);
    function GetCurrent: TQMsgPack; inline;
    function MoveNext: Boolean;
    property Current: TQMsgPack read GetCurrent;
  end;

  TQMsgPack = class
  private
    FName: QStringW; // 名称
    FNameHash: Cardinal; // 哈希值
    FValue: TBytes; // 值
    FItems: TQMsgPackList;
    FParent: TQMsgPack;
    FDataType: TQMsgPackType;
    FExtType: Shortint;
    FData: Pointer;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsMsgPack: TBytes;
    function GetAsString: QStringW;
    function GetAsVariant: Variant;
    function GetCount: Integer;
    function GetIsArray: Boolean;
    function GetIsDateTime: Boolean;
    function GetIsNull: Boolean;
    function GetIsNumeric: Boolean;
    function GetIsObject: Boolean;
    function GetIsString: Boolean;
    function GetItemIndex: Integer;
    function GetItems(AIndex: Integer): TQMsgPack;
    function GetPath: QStringW;
    function GetValue: QStringW;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsMsgPack(const Value: TBytes);
    procedure SetAsString(const Value: QStringW);
    procedure SetAsVariant(const Value: Variant);
    procedure SetDataType(const Value: TQMsgPackType);
    procedure InternalParse(var p: PByte; l: Integer);
    procedure ArrayNeeded(ANewType: TQMsgPackType);
    function CreateItem: TQMsgPack; virtual;
    procedure FreeItem(AItem: TQMsgPack); virtual;
    procedure CopyValue(ASource: TQMsgPack); inline;
    procedure SetExtType(const Value: Shortint);
    function GetAsExtBytes: TBytes;
    procedure SetExtBytes(const Value: TBytes);
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);
    function GetAsSingle: Single;
    procedure SetAsSingle(const Value: Single);
  protected
    procedure Replace(AIndex: Integer; ANewItem: TQMsgPack); virtual;
  public
    /// <summary>构造函数</summary>
    constructor Create; overload;
    constructor Create(const AName: QStringW;
      ADataType: TQMsgPackType = mptUnknown); overload;
    /// <summary>析构函数</summary>
    destructor Destroy; override;
    { <summary》添加一个子结点<、summary>
      <param name="ANode">要添加的结点</param>
      <returns>返回添加的结点索引</returns>
    }
    function Add(ANode: TQMsgPack): Integer; overload;
    /// <summary>添加一个未命名的MsgPack子结点</summary>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>
    /// 一般情况下，除非数组类型，不应添加未命名的实例
    /// </remarks>
    function Add: TQMsgPack; overload;
    /// <summary>添加一个数组</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AItems">要添加的数组内容</param>
    /// <returns>返回创建的结点实例</returns>
    function Add(const AName: QStringW; AItems: array of const)
      : TQMsgPack; overload;
    { <summary>添加一个子结点</summary>
      <param name="AName">要添加的结点名</param>
      <param name="ADataType">要添加的结点数据类型，如果省略，则自动根据值的内容检测</param>
      <returns>返回添加的新对象</returns>
      <remarks>
      1.如果当前类型不是jdtObject或jdtArray，将自动转换为jdtObject类型
      2.上层应自己负责重名检查
      </remarks>
    }
    function Add(AName: QStringW; ADataType: TQMsgPackType): TQMsgPack;
      overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName, AValue: QStringW): TQMsgPack; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Double): TQMsgPack; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Int64): TQMsgPack; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; AValue: Boolean): TQMsgPack; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW; const AValue: TBytes): TQMsgPack; overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function AddDateTime(AName: QStringW; AValue: TDateTime)
      : TQMsgPack; overload;
    /// <summary>添加一个子结点(Null)</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName: QStringW): TQMsgPack; overload; virtual;

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
    function ForcePath(APath: QStringW): TQMsgPack;
    /// <summary>解析指定的MsgPack字节序列</summary>
    /// <param name="p">要解析的字节序列</param>
    /// <param name="l">字符串长度，<=0认为是以\0(#0)结尾的C语言标准字符串</param>
    /// <remarks>如果l>=0，会检测p[l]是否为\0，如果不为\0，则会创建拷贝实例并解析拷贝实例</remarks>
    procedure Parse(p: PByte; l: Integer = -1); overload;
    /// <summary>解析指定的MsgPack字符串</summary>
    /// <param name="s">要解析的MsgPack字符串</param>
    procedure Parse(const s: TBytes); overload;
    /// <summary>拷贝生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function Copy: TQMsgPack;
{$IFDEF UNICODE}
    /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function CopyIf(const ATag: Pointer; AFilter: TQMsgPackFilterEventA)
      : TQMsgPack; overload;
{$ENDIF UNICODE}
    /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function CopyIf(const ATag: Pointer; AFilter: TQMsgPackFilterEvent)
      : TQMsgPack; overload;
    /// <summary>克隆生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为实际上执行的是拷贝，所以新旧对象之间的内容变更没有任何关系，
    /// 更改任意一个对象，不会对另外一个对象造成影响，但此行为将来并不保证，可能
    /// 会调整为引用，以便相互影响。
    /// </remarks>
    function Clone: TQMsgPack;
    /// <summary>编码</summary>
    /// <returns>返回编码后的字节流</returns>
    /// <remarks>AsMsgPack等价于本函数</remarks>
    function Encode: TBytes;
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
    /// <remarks>注意QMsgPack并不检查重名，因此，如果存在重名的结点，只会返回第一个结点</remarks>
    function ItemByName(AName: QStringW): TQMsgPack; overload;
    /// <summary>获取指定名称的结点到列表中</summary>
    /// <param name="AName">结点名称</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByName(const AName: QStringW; AList: TQMsgPackList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取符合指定名称规则的结点到列表中</summary>
    /// <param name="ARegex">正则表达式</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByRegex(const ARegex: QStringW; AList: TQMsgPackList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取指定路径的MsgPack对象</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <returns>返回找到的子结点，如果未找到返回NULL(nil)</returns>
    function ItemByPath(APath: QStringW): TQMsgPack;
    /// <summary>从源对象复制MsgPack对象内容</summary>
    /// <param name="ANode">要复制的源结点</param>
    /// <remarks>注意不要复制子结点给自己，否则会造成死循环。要复制子结点，先复
    /// 制一个子结点的新实例，再从新实例复制
    /// </remarks>
    procedure Assign(ANode: TQMsgPack); virtual;
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
      AFilter: TQMsgPackFilterEventA); overload;
{$ENDIF UNICODE}
    /// <summary>
    /// 删除符合条件的子结点
    /// </summary>
    /// <param name="ATag">用户自己附加的额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，等价于Clear</param>
    procedure DeleteIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQMsgPackFilterEvent); overload;
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
      AFilter: TQMsgPackFilterEventA): TQMsgPack; overload;
{$ENDIF UNICODE}
    /// <summary>遍历结点查找符合条件的结点</summary>
    /// <param name="ATag">用户自定义的附加额外标记</param>
    /// <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    /// <param name="AFilter">过滤回调函数，如果为nil，则返回nil</param>
    function FindIf(const ATag: Pointer; ANest: Boolean;
      AFilter: TQMsgPackFilterEvent): TQMsgPack; overload;
    /// <summary>清除所有的结点</summary>
    procedure Clear; virtual;
    /// <summary>保存当前对象内容到流中</summary>
    /// <param name="AStream">目标流对象</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBom">是否写入BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToStream(AStream: TStream);
    /// <summary>从流的当前位置开始加载MsgPack对象</summary>
    /// <param name="AStream">源数据流</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    /// <remarks>流的当前位置到结束的长度必需大于2字节，否则无意义</remarks>
    procedure LoadFromStream(AStream: TStream);
    /// <summary>保存当前对象内容到文件中</summary>
    /// <param name="AFileName">文件名</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBOM">是否写入UTF-8的BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToFile(AFileName: String);
    /// <summary>从指定的文件中加载当前对象</summary>
    /// <param name="AFileName">要加载的文件名</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    procedure LoadFromFile(AFileName: String);
    /// / <summary>重置值为Null，等价于直接设置DataType为jdtNull</summary>
    procedure ResetNull;
    /// <summary>重载TObject.ToString函数</summary>
    function ToString: string; {$IFDEF UNICODE}override; {$ELSE}virtual;
{$ENDIF}
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
    function GetEnumerator: TQMsgPackEnumerator;
    /// <summary>判断自己是否是一个指定的对象的子对象</summary>
    /// <param name="AParent">可能的父对象</param>
    /// <returns>如果自己是指定对象的子对象，则返回True，否则，返回False</returns>
    function IsChildOf(AParent: TQMsgPack): Boolean;
    /// <summary>判断自己是否是一个指定的对象的父对象</summary>
    /// <param name="AParent">可能的子对象</param>
    /// <returns>如果自己是指定对象的父对象，则返回True，否则，返回False</returns>
    function IsParentOf(AChild: TQMsgPack): Boolean;
    /// <summary>从流中加载二进制数据</summary>
    /// <param name="AStream">源数据流</param>
    /// <param name="ACount">要拷贝的字节数，如果为0，则拷贝源数据流的全部内容</param>
    procedure BytesFromStream(AStream: TStream; ACount: Integer);
    /// <summary>从文件中加载二进制数据</summary>
    /// <param name="AFileName">源文件名</param>
    procedure BytesFromFile(AFileName: QStringW);
    /// <summary>将当前数据保存到流中</summary>
    /// <param name="AStream">目标数据流</param>
    procedure BytesToStream(AStream: TStream);
    /// <summary>将当前数据保存到文件中</summary>
    /// <param name="AFileName">目标文件名</param>
    procedure BytesToFile(AFileName: QStringW);
    /// <summary>父结点</summary>
    property Parent: TQMsgPack read FParent;
    /// <summary>结点类型</summary>
    /// <seealso>TQMsgPackType</seealso>
    property DataType: TQMsgPackType read FDataType write SetDataType;
    /// <summary>结点名称</summary>
    property Name: QStringW read FName;
    /// <summary>子结点数量</<summary>summary>
    property Count: Integer read GetCount;
    /// <summary>子结点数组</summary>
    property Items[AIndex: Integer]: TQMsgPack read GetItems; default;
    /// <summary>子结点的值</summary>
    property Value: QStringW read GetValue;
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
    /// <summary>将当前结点作为字节流来访问</summary>
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    /// <summary>将当前结点当作整数类型来访问</summary>
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    /// <summary>将当前结点当作64位整数类型来访问</summary>
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    /// <summary>将当前结点当作双浮点类型来访问</summary>
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    /// <summary>将当前结点当作单精度浮点类型来访问</summary>
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    /// <summary>将当前结点当作日期时间类型来访问</summary>
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    /// <summary>将当前结点当作字符串类型访问</summary>
    property AsString: QStringW read GetAsString write SetAsString;
    /// <summary>将自己当做Variant类型来访问</summary>
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    /// <summary>将自己当做MsgPack序列来访问</summary>
    property AsMsgPack: TBytes read GetAsMsgPack write SetAsMsgPack;
    /// <summary>将自己当做扩展对象来访问</summary>
    property AsExtBytes: TBytes read GetAsExtBytes write SetExtBytes;
    // <summary>额外的附加数据成员，供用户关联附加内容</summary>
    property Data: Pointer read FData write FData;
    /// <summary>结点的路径，路径中间以"\"分隔</summary>
    property Path: QStringW read GetPath;
    /// <summary>在父结点中的索引顺序，从0开始，如果是-1，则代表自己是根结点</summary>
    property ItemIndex: Integer read GetItemIndex;
    /// <summary>名称哈希值</summary>
    property NameHash: Cardinal read FNameHash;
    /// <summary>扩展类型</summary>
    property ExtType: Shortint read FExtType write SetExtType;
  end;

  TQHashedMsgPack = class(TQMsgPack)
  protected
    FHashTable: TQHashTable;
    function CreateItem: TQMsgPack; override;
    procedure Replace(AIndex: Integer; ANewItem: TQMsgPack); override;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(ANode: TQMsgPack); override;
    function Add(AName: QStringW): TQMsgPack; override;
    function IndexOf(const AName: QStringW): Integer; override;
    procedure Delete(AIndex: Integer); override;
    procedure Clear; override;
  end;

  /// <summary>用于外部支持对象池的函数，创建一个新的TQMsgPack对象，注意从池中创建的对象</summary>
  /// <returns>返回新创建的TQMsgPack对象</returns>
  TQMsgPackCreateEvent = function: TQMsgPack;
  /// <summary>用于外部将对象缓存，以便重用对象</summary>
  /// <param name="AObj">要释放的MsgPack对象</param>
  TQMsgPackFreeEvent = procedure(AObj: TQMsgPack);

var
  /// <summary>日期类型转换为字符串时，这个变量控制如何格式化</summary>
  MsgPackDateFormat: QStringW;
  /// <summary>时间类型转换为字符串时，这个变量控制如何格式化</summary>
  MsgPackTimeFormat: QStringW;
  /// <summary>日期时间类型转换为字符串时，这个变量控制如何格式化</summary>
  MsgPackDateTimeFormat: QStringW;
  /// <summary>在ItemByName/ItemByPath/ValueByName/ValueByPath等函数的判断中，是否区分名称大小写</summary>
  MsgPackCaseSensitive: Boolean;
  /// <summary>指定如何处理RTTI中的枚举和集合类型</summary>
  MsgPackRttiEnumAsInt: Boolean;
  /// 在需要新建一个TQMsgPack对象时触发
  OnQMsgPackCreate: TQMsgPackCreateEvent;
  /// 在需要释放一个TQMsgPack对象时触发
  OnQMsgPackFree: TQMsgPackFreeEvent;
  QMsgPackPathDelimiter: QCharW = '\';

implementation

resourcestring

  SNotArrayOrMap = '%s 不是映射或数组。';
  SUnsupportArrayItem = '添加的动态数组第%d个元素类型不受支持。';
  SBadMsgPackArray = '%s 不是一个有效的MsgPack数组定义。';
  SBadMsgPackName = '%s 不是一个有效的MsgPack名称。';
  SBadConvert = '%s 不是一个有效的 %s 类型的值。';
  SVariantNotSupport = '不支持转换为Variant类型。';
  SNotSupport = '函数 [%s] 在当前开发环境下不受支持。';
  SReservedExtType = '<0的扩展类型被标准声明为保留不可用。';
  SReplaceTypeNeed = '替换结点的类型要求是 %s 或其子类。';
  SParamMissed = '参数 %s 同名的结点未找到。';
  SMethodMissed = '指定的函数 %s 不存在。';
  SMissRttiTypeDefine =
    '无法找到 %s 的RTTI类型信息，尝试将对应的类型单独定义(如array[0..1] of Byte改为TByteArr=array[0..1]，然后用TByteArr声明)。';
  SUnsupportPropertyType = '不支持的属性类型';
  SUnsupportValueType = 'TValue不支持二进制或扩展类型.';
  SArrayTypeMissed = '未知的数组元素类型。';
  SMapNameMissed = '映射名称未找到，无效和MessagePack格式？';

type

  TQMsgPackValue = packed record
    ValueType: Byte;
    case Integer of
      0:
        (U8Val: Byte);
      1:
        (I8Val: Shortint);
      2:
        (U16Val: Word);
      3:
        (I16Val: Smallint);
      4:
        (U32Val: Cardinal);
      5:
        (I32Val: Integer);
      6:
        (U64Val: UInt64);
      7:
        (I64Val: Int64);
      8:
        (F32Val: Single);
      9:
        (F64Val: Double);
      10:
        (BArray: array [0 .. 16] of Byte);
  end;

const
  MsgPackTypeName: array [0 .. 10] of QStringW = ('Unknown', 'Integer', 'Null',
    'Boolean', 'Float', 'String', 'Binary', 'Array', 'Map', 'Extended',
    'DateTime');
  { TQMsgPack }

constructor TQMsgPack.Create;
begin
inherited;
end;

constructor TQMsgPack.Create(const AName: QStringW; ADataType: TQMsgPackType);
begin
FName := AName;
DataType := ADataType;
end;

function TQMsgPack.CreateItem: TQMsgPack;
begin
if Assigned(OnQMsgPackCreate) then
  Result := OnQMsgPackCreate
else
  Result := TQMsgPack.Create;
end;

procedure TQMsgPack.Delete(AName: QStringW);
var
  I: Integer;
begin
I := IndexOf(AName);
if I <> -1 then
  Delete(I);
end;

procedure TQMsgPack.Delete(AIndex: Integer);
begin
if FDataType in [mptArray, mptMap] then
  begin
  FreeItem(Items[AIndex]);
  FItems.Delete(AIndex);
  end
else
  raise Exception.Create(Format(SNotArrayOrMap, [FName]));
end;

procedure TQMsgPack.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQMsgPackFilterEvent);
  procedure DeleteChildren(AParent: TQMsgPack);
  var
    I: Integer;
    Accept: Boolean;
    AChild: TQMsgPack;
  begin
  I := 0;
  while I < AParent.Count do
    begin
    Accept := True;
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
{$IFDEF UNICODE}

procedure TQMsgPack.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQMsgPackFilterEventA);
  procedure DeleteChildren(AParent: TQMsgPack);
  var
    I: Integer;
    Accept: Boolean;
    AChild: TQMsgPack;
  begin
  I := 0;
  while I < AParent.Count do
    begin
    Accept := True;
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

destructor TQMsgPack.Destroy;
begin
if DataType in [mptArray, mptMap] then
  begin
  Clear;
  FreeObject(FItems);
  end;
inherited;
end;

function TQMsgPack.Add(const AName: QStringW; AItems: array of const)
  : TQMsgPack;
var
  I: Integer;
begin
Result := Add(AName, mptArray);
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
      if TObject(AItems[I].VObject) is TQMsgPack then
        Result.Add(TObject(AItems[I].VObject) as TQMsgPack)
      else
        raise Exception.Create(Format(SUnsupportArrayItem, [I]));
      end
  else
    raise Exception.Create(Format(SUnsupportArrayItem, [I]));
  end; // End case
  end; // End for
end;

function TQMsgPack.Add(AName: QStringW; ADataType: TQMsgPackType): TQMsgPack;
begin
Result := Add(AName);
Result.DataType := ADataType;
end;

function TQMsgPack.Add: TQMsgPack;
begin
ArrayNeeded(mptMap);
Result := TQMsgPack.Create;
Result.FParent := Self;
FItems.Add(Result);
end;

function TQMsgPack.Add(AName: QStringW; AValue: Boolean): TQMsgPack;
begin
Result := Add(AName);
Result.AsBoolean := AValue;
end;

function TQMsgPack.Add(AName: QStringW): TQMsgPack;
begin
Result := Add;
Result.FName := AName;
end;

function TQMsgPack.Add(AName: QStringW; const AValue: TBytes): TQMsgPack;
begin
Result := Add(AName);
Result.DataType := mptBinary;
Result.FValue := AValue;
end;

function TQMsgPack.Add(AName, AValue: QStringW): TQMsgPack;
begin
Result := Add(AName);
Result.AsString := AValue;
end;

function TQMsgPack.Add(AName: QStringW; AValue: Double): TQMsgPack;
begin
Result := Add(AName);
Result.AsFloat := AValue;
end;

function TQMsgPack.Add(AName: QStringW; AValue: Int64): TQMsgPack;
begin
Result := Add(AName);
Result.AsInt64 := AValue;
end;

function TQMsgPack.Add(ANode: TQMsgPack): Integer;
begin
ArrayNeeded(mptArray);
Result := FItems.Add(ANode);
end;

function TQMsgPack.AddDateTime(AName: QStringW; AValue: TDateTime): TQMsgPack;
begin
Result := Add(AName);
Result.AsDateTime := AValue;
end;

procedure TQMsgPack.ArrayNeeded(ANewType: TQMsgPackType);
begin
if not(DataType in [mptArray, mptMap]) then
  DataType := ANewType;
end;

procedure TQMsgPack.Assign(ANode: TQMsgPack);
var
  I: Integer;
  AItem, ACopy: TQMsgPack;
begin
if ANode.FDataType in [mptArray, mptMap] then
  begin
  DataType := ANode.FDataType;
  if Count>0 then
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

procedure TQMsgPack.Clear;
var
  I: Integer;
begin
if FDataType in [mptArray, mptMap] then
  begin
  for I := 0 to Count - 1 do
    FreeItem(FItems[I]);
  FItems.Clear;
  end;
end;

function TQMsgPack.Clone: TQMsgPack;
begin
Result := Copy;
end;

function TQMsgPack.Copy: TQMsgPack;
begin
Result := CreateItem;
Result.Assign(Self);
end;
{$IFDEF UNICODE}

function TQMsgPack.CopyIf(const ATag: Pointer; AFilter: TQMsgPackFilterEventA)
  : TQMsgPack;
  procedure NestCopy(AParentSource, AParentDest: TQMsgPack);
  var
    I: Integer;
    Accept: Boolean;
    AChildSource, AChildDest: TQMsgPack;
  begin
  for I := 0 to AParentSource.Count - 1 do
    begin
    Accept := True;
    AChildSource := AParentSource[I];
    AFilter(Self, AChildSource, Accept, ATag);
    if Accept then
      begin
      AChildDest := AParentDest.Add(AChildSource.FName, AChildSource.DataType);
      if AChildSource.DataType in [mptArray, mptMap] then
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
  Result := CreateItem;
  Result.FName := Name;
  if DataType in [mptArray, mptMap] then
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

function TQMsgPack.CopyIf(const ATag: Pointer; AFilter: TQMsgPackFilterEvent)
  : TQMsgPack;
  procedure NestCopy(AParentSource, AParentDest: TQMsgPack);
  var
    I: Integer;
    Accept: Boolean;
    AChildSource, AChildDest: TQMsgPack;
  begin
  for I := 0 to AParentSource.Count - 1 do
    begin
    Accept := True;
    AChildSource := AParentSource[I];
    AFilter(Self, AChildSource, Accept, ATag);
    if Accept then
      begin
      AChildDest := AParentDest.Add(AChildSource.FName, AChildSource.DataType);
      if AChildSource.DataType in [mptArray, mptMap] then
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
  Result := CreateItem;
  Result.FName := Name;
  if DataType in [mptArray, mptMap] then
    begin
    NestCopy(Self, Result);
    end
  else
    Result.CopyValue(Self);
  end
else
  Result := Copy;
end;

procedure TQMsgPack.CopyValue(ASource: TQMsgPack);
var
  L: Integer;
begin
L := Length(ASource.FValue);
DataType := ASource.DataType;
if DataType in [mptString, mptBinary, mptExtended] then
  SetLength(FValue, L);
if L>0 then
  begin
  if not (DataType in [mptUnknown,mptNull,mptArray,mptMap]) then
    Move(ASource.FValue[0], FValue[0], L);
  end;
end;

function TQMsgPack.Encode: TBytes;
var
  AStream: TMemoryStream;
  AValue: TQMsgPackValue;

  procedure WriteNull;
  begin
  AValue.U8Val := $C0;
  AStream.Write(AValue.U8Val, 1);
  end;

  procedure LE2BE(ASize: Integer);
  var
    I, C: Integer;
    B: Byte;
  begin
  C := ASize shr 1;
  for I := 0 to C - 1 do
    begin
    B := AValue.BArray[I];
    AValue.BArray[I] := AValue.BArray[ASize - I - 1];
    AValue.BArray[ASize - I - 1] := B;
    end;
  end;

  procedure WriteInt(const iVal: Int64);
  begin
  if iVal >= 0 then
    begin
    if iVal <= 127 then
      begin
      AValue.U8Val := Byte(iVal);
      AStream.WriteBuffer(AValue.U8Val, 1);
      end
    else if iVal <= 255 then // UInt8
      begin
      AValue.ValueType := $CC;
      AValue.U8Val := Byte(iVal);
      AStream.WriteBuffer(AValue, 2);
      end
    else if iVal <= 65535 then
      begin
      AValue.ValueType := $CD;
      AValue.BArray[0] := (iVal shr 8);
      AValue.BArray[1] := (iVal and $FF);
      AStream.WriteBuffer(AValue, 3);
      end
    else if iVal <= Cardinal($FFFFFFFF) then
      begin
      AValue.ValueType := $CE;
      AValue.BArray[0] := (iVal shr 24) and $FF;
      AValue.BArray[1] := (iVal shr 16) and $FF;
      AValue.BArray[2] := (iVal shr 8) and $FF;
      AValue.BArray[3] := iVal and $FF;
      AStream.WriteBuffer(AValue, 5);
      end
    else
      begin
      AValue.ValueType := $CF;
      AValue.BArray[0] := (iVal shr 56) and $FF;
      AValue.BArray[1] := (iVal shr 48) and $FF;
      AValue.BArray[2] := (iVal shr 40) and $FF;
      AValue.BArray[3] := (iVal shr 32) and $FF;
      AValue.BArray[4] := (iVal shr 24) and $FF;
      AValue.BArray[5] := (iVal shr 16) and $FF;
      AValue.BArray[6] := (iVal shr 8) and $FF;
      AValue.BArray[7] := iVal and $FF;
      AStream.WriteBuffer(AValue, 9);
      end;
    end
  else // <0
    begin
    if iVal <= -2147483648 then // 64位
      begin
      AValue.ValueType := $D3;
      AValue.BArray[0] := (iVal shr 56) and $FF;
      AValue.BArray[1] := (iVal shr 48) and $FF;
      AValue.BArray[2] := (iVal shr 40) and $FF;
      AValue.BArray[3] := (iVal shr 32) and $FF;
      AValue.BArray[4] := (iVal shr 24) and $FF;
      AValue.BArray[5] := (iVal shr 16) and $FF;
      AValue.BArray[6] := (iVal shr 8) and $FF;
      AValue.BArray[7] := iVal and $FF;
      AStream.WriteBuffer(AValue, 9);
      end
    else if iVal <= -32768 then
      begin
      AValue.ValueType := $D2;
      AValue.BArray[0] := (iVal shr 24) and $FF;
      AValue.BArray[1] := (iVal shr 16) and $FF;
      AValue.BArray[2] := (iVal shr 8) and $FF;
      AValue.BArray[3] := iVal and $FF;
      AStream.WriteBuffer(AValue, 5);
      end
    else if iVal <= -128 then
      begin
      AValue.ValueType := $D1;
      AValue.BArray[0] := (iVal shr 8);
      AValue.BArray[1] := (iVal and $FF);
      AStream.WriteBuffer(AValue, 3);
      end
    else if iVal < -32 then
      begin
      AValue.ValueType := $D0;
      AValue.I8Val := iVal;
      AStream.WriteBuffer(AValue, 2);
      end
    else
      begin
      AValue.I8Val := iVal;
      AStream.Write(AValue.I8Val, 1);
      end;
    end; // End <0
  end;

  procedure WriteFloat(fVal: Extended);
  begin
  AValue.F64Val := fVal;
  LE2BE(SizeOf(Double));
  AValue.ValueType := $CB;
  AStream.WriteBuffer(AValue, 9);
  end;
  procedure WriteSingle(fVal: Extended);
  begin
  AValue.F32Val := fVal;
  LE2BE(SizeOf(Single));
  AValue.ValueType := $CA;
  AStream.WriteBuffer(AValue, 5);
  end;
  procedure WriteString(const s: QStringW);
  var
    U: QStringA;
    l: Integer;
  begin
  U := qstring.Utf8Encode(s);
  l := U.Length;
  if l <= 31 then
    begin
    AValue.ValueType := $A0 + Byte(l);
    AStream.WriteBuffer(AValue.ValueType, 1);
    end
  else if l <= 255 then
    begin
    AValue.ValueType := $D9;
    AValue.U8Val := Byte(l);
    AStream.WriteBuffer(AValue, 2);
    end
  else if l <= 65535 then
    begin
    AValue.ValueType := $DA;
    AValue.U16Val := ((l shr 8) and $FF) or ((l shl 8) and $FF00);
    AStream.Write(AValue, 3);
    end
  else
    begin
    AValue.ValueType := $DB;
    AValue.BArray[0] := (l shr 24) and $FF;
    AValue.BArray[1] := (l shr 16) and $FF;
    AValue.BArray[2] := (l shr 8) and $FF;
    AValue.BArray[3] := l and $FF;
    AStream.WriteBuffer(AValue, 5);
    end;
  AStream.WriteBuffer(PQCharA(U)^, l);
  end;

  procedure WriteBinary(p: PByte; l: Integer);
  begin
  if l <= 255 then
    begin
    AValue.ValueType := $C4;
    AValue.U8Val := Byte(l);
    AStream.WriteBuffer(AValue, 2);
    end
  else if l <= 65535 then
    begin
    AValue.ValueType := $C5;
    AValue.BArray[0] := (l shr 8) and $FF;
    AValue.BArray[1] := l and $FF;
    AStream.WriteBuffer(AValue, 3);
    end
  else
    begin
    AValue.ValueType := $C6;
    AValue.BArray[0] := (l shr 24) and $FF;
    AValue.BArray[1] := (l shr 16) and $FF;
    AValue.BArray[2] := (l shr 8) and $FF;
    AValue.BArray[3] := l and $FF;
    AStream.WriteBuffer(AValue, 5);
    end;
  AStream.WriteBuffer(p^, l);
  end;

  procedure InternalEncode(ANode: TQMsgPack; AStream: TMemoryStream);
  var
    I, ANewSize, C: Integer;
    AChild: TQMsgPack;
  begin
  if AStream.Position = AStream.Size then
    begin
    ANewSize := 0;
    while AStream.Position + ANewSize <= AStream.Size do // Try+16K
      Inc(ANewSize, 16384);
    AStream.Size := AStream.Size + ANewSize;
    end;
  case ANode.DataType of
    mptUnknown, mptNull:
      WriteNull;
    mptInteger:
      WriteInt(ANode.AsInt64);
    mptBoolean:
      begin
      if ANode.AsBoolean then
        AValue.U8Val := $C3
      else
        AValue.U8Val := $C2;
      AStream.WriteBuffer(AValue.U8Val, 1);
      end;
    mptDateTime, mptFloat:
      WriteFloat(ANode.AsFloat);
    mptSingle:
      WriteSingle(ANode.AsSingle);
    mptString:
      WriteString(ANode.AsString);
    mptBinary:
      WriteBinary(@ANode.FValue[0], Length(ANode.FValue));
    mptArray:
      begin
      C := ANode.Count;
      if C <= 15 then
        begin
        AValue.ValueType := $90 + C;
        AStream.WriteBuffer(AValue.ValueType, 1);
        end
      else if C <= 65535 then
        begin
        AValue.ValueType := $DC;
        AValue.BArray[0] := (C shr 8) and $FF;
        AValue.BArray[1] := C and $FF;
        AStream.WriteBuffer(AValue, 3);
        end
      else
        begin
        AValue.ValueType := $DD;
        AValue.BArray[0] := (C shr 24) and $FF;
        AValue.BArray[1] := (C shr 16) and $FF;
        AValue.BArray[2] := (C shr 8) and $FF;
        AValue.BArray[3] := C and $FF;
        AStream.WriteBuffer(AValue, 5);
        end;
      for I := 0 to C - 1 do
        InternalEncode(ANode[I], AStream);
      end;
    mptMap:
      begin
      C := ANode.Count;
      if C <= 15 then
        begin
        AValue.ValueType := $80 + C;
        AStream.WriteBuffer(AValue.ValueType, 1);
        end
      else if C <= 65535 then
        begin
        AValue.ValueType := $DE;
        AValue.BArray[0] := (C shr 8) and $FF;
        AValue.BArray[1] := C and $FF;
        AStream.WriteBuffer(AValue, 3);
        end
      else
        begin
        AValue.ValueType := $DF;
        AValue.BArray[0] := (C shr 24) and $FF;
        AValue.BArray[1] := (C shr 16) and $FF;
        AValue.BArray[2] := (C shr 8) and $FF;
        AValue.BArray[3] := C and $FF;
        AStream.WriteBuffer(AValue, 5);
        end;
      for I := 0 to C - 1 do
        begin
        AChild := ANode[I];
        WriteString(AChild.FName);
        InternalEncode(AChild, AStream);
        end;
      end;
    mptExtended:
      begin
      C := Length(ANode.FValue);
      if C = 1 then
        begin
        AValue.ValueType := $D4;
        AValue.BArray[0] := ANode.FExtType;
        AValue.BArray[1] := ANode.FValue[0];
        AStream.WriteBuffer(AValue, 3);
        end
      else if C = 2 then
        begin
        AValue.ValueType := $D5;
        AValue.BArray[0] := ANode.FExtType;
        AValue.BArray[1] := ANode.FValue[0];
        AValue.BArray[2] := ANode.FValue[1];
        AStream.WriteBuffer(AValue, 4);
        end
      else if C = 4 then
        begin
        AValue.ValueType := $D6;
        AValue.BArray[0] := ANode.FExtType;
        PInteger(@AValue.BArray[1])^ := PInteger(@ANode.FValue[0])^;
        AStream.WriteBuffer(AValue, 6);
        end
      else if C = 8 then
        begin
        AValue.ValueType := $D7;
        AValue.BArray[0] := ANode.FExtType;
        PInt64(@AValue.BArray[1])^ := PInt64(@ANode.FValue[0])^;
        AStream.WriteBuffer(AValue, 10);
        end
      else if C = 16 then
        begin
        AValue.ValueType := $D8;
        AValue.BArray[0] := ANode.FExtType;
        PInt64(@AValue.BArray[1])^ := PInt64(@ANode.FValue[0])^;
        PInt64(@AValue.BArray[9])^ := PInt64(@ANode.FValue[8])^;
        AStream.WriteBuffer(AValue, 18);
        end
      else if C <= 255 then
        begin
        AValue.ValueType := $C7;
        AValue.BArray[0] := Byte(C);
        AValue.BArray[1] := ANode.FExtType;
        AStream.WriteBuffer(AValue, 3);
        AStream.WriteBuffer(ANode.FValue[0], C);
        end
      else if C <= 65535 then
        begin
        AValue.ValueType := $C8;
        AValue.BArray[0] := (C shr 8) and $FF;
        AValue.BArray[1] := (C and $FF);
        AValue.BArray[2] := ANode.FExtType;
        AStream.WriteBuffer(AValue, 4);
        AStream.WriteBuffer(ANode.FValue[0], C);
        end
      else
        begin
        AValue.ValueType := $C8;
        AValue.BArray[0] := (C shr 24) and $FF;
        AValue.BArray[1] := (C shr 16) and $FF;
        AValue.BArray[2] := (C shr 8) and $FF;
        AValue.BArray[3] := (C and $FF);
        AValue.BArray[4] := ANode.FExtType;
        AStream.WriteBuffer(AValue, 4);
        end;
      end;
  end;
  end;

begin
AStream := TMemoryStream.Create;
AStream.Size := 16384;
try
  InternalEncode(Self, AStream);
  SetLength(Result, AStream.Position);
  if Length(Result) > 0 then
    Move(PByte(AStream.Memory)^, Result[0], Length(Result));
finally
  FreeObject(AStream);
end;
end;

function TQMsgPack.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQMsgPackFilterEvent): TQMsgPack;
  function DoFind(AParent: TQMsgPack): TQMsgPack;
  var
    I: Integer;
    AChild: TQMsgPack;
    Accept: Boolean;
  begin
  Result := nil;
  for I := 0 to AParent.Count - 1 do
    begin
    AChild := AParent[I];
    Accept := True;
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
{$IFDEF UNICODE}

function TQMsgPack.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQMsgPackFilterEventA): TQMsgPack;
  function DoFind(AParent: TQMsgPack): TQMsgPack;
  var
    I: Integer;
    AChild: TQMsgPack;
    Accept: Boolean;
  begin
  Result := nil;
  for I := 0 to AParent.Count - 1 do
    begin
    AChild := AParent[I];
    Accept := True;
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

function TQMsgPack.ForcePath(APath: QStringW): TQMsgPack;
var
  AName: QStringW;
  p, pn, ws: PQCharW;
  AParent: TQMsgPack;
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
  AName := DecodeTokenW(p, PathDelimiters, WideChar(0), True);
  if not(AParent.DataType in [mptArray, mptMap]) then
    AParent.DataType := mptMap;
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
          Result := AParent.Add(AName, mptArray)
        else if Result.DataType <> mptArray then
          raise Exception.CreateFmt(SBadMsgPackArray, [AName]);
        if AIndex >= 0 then
          begin
          while Result.Count <= AIndex do
            Result.Add;
          Result := Result[AIndex];
          end;
        end
      else
        raise Exception.CreateFmt(SBadMsgPackName, [AName]);
      end
    else
      Result := AParent.Add(AName);
    end;
  AParent := Result;
  end;
end;

procedure TQMsgPack.FreeItem(AItem: TQMsgPack);
begin
if Assigned(OnQMsgPackFree) then
  OnQMsgPackFree(AItem)
else
  FreeObject(AItem);
end;

{$IFDEF UNICODE}

procedure TQMsgPack.FromRecord<T>(const ARecord: T);
begin
FromRtti(@ARecord, TypeInfo(T));
end;

procedure TQMsgPack.FromRtti(ASource: Pointer; AType: PTypeInfo);
var
  AValue: TValue;
  procedure AddCollection(AParent: TQMsgPack; ACollection: TCollection);
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
      Result:=0;
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
          else if MsgPackRttiEnumAsInt then
            Add(AFields[J].Name).AsInt64 := AFields[J].GetValue(ASource)
              .AsOrdinal
          else
            Add(AFields[J].Name).AsString :=
              AFields[J].GetValue(ASource).ToString;
          end;
        tkSet:
          begin
          if MsgPackRttiEnumAsInt then
            // Add(AFields[J].Name).AsInteger := AFields[J].GetValue(ASource).AsInteger
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
          with Add(AFields[J].Name, mptArray) do
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
            AddCollection(Add(AFields[J].Name, mptArray), AObj as TCollection)
          else // 其它类型的对象不保存
            Add(AFields[J].Name).FromRtti(AObj, AFields[J].FieldType.Handle);
          end;
        tkRecord:
          begin
          DataType := mptMap;
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
            else if MsgPackRttiEnumAsInt then
              Add(AName).AsInteger := GetOrdProp(AObj, APropList[J])
            else
              Add(AName).AsString := GetEnumProp(AObj, APropList[J]);
            end;
          tkSet:
            if MsgPackRttiEnumAsInt then
              Add(AName).AsInteger := GetOrdProp(AObj, APropList[J])
            else
              Add(AName).AsString := GetSetProp(AObj, APropList[J], True);
          tkVariant:
            Add(AName).AsVariant := GetPropValue(AObj, APropList[J]);
          tkInt64:
            Add(AName).AsInt64 := GetInt64Prop(AObj, APropList[J]);
        end;
        end;
      end;
    end;
  end;
  procedure AddArray;
  var
    I, C: Integer;
  begin
  DataType := mptArray;
  Clear;
  TValue.Make(ASource, AType, AValue);
  C := AValue.GetArrayLength;
  for I := 0 to C - 1 do
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

procedure TQMsgPack.FromRtti(AInstance: TValue);
var
  I, C: Integer;
begin
case AInstance.Kind of
  tkClass:
    FromRtti(AInstance.AsObject, AInstance.TypeInfo);
  tkRecord:
    FromRtti(AInstance.GetReferenceToRawData, AInstance.TypeInfo);
  tkArray, tkDynArray:
    begin
    DataType := mptArray;
    Clear;
    C := AInstance.GetArrayLength;
    for I := 0 to C - 1 do
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
    else if MsgPackRttiEnumAsInt then
      AsInt64 := AInstance.AsOrdinal
    else
      AsString := AInstance.ToString;
    end;
  tkSet:
    begin
    if MsgPackRttiEnumAsInt then
      AsInt64 := AInstance.AsOrdinal
    else
      AsString := AInstance.ToString;
    end;
  tkVariant:
    AsVariant := AInstance.AsVariant;
  tkInt64:
    AsInt64 := AInstance.AsInt64;
end;
end;
{$ENDIF UNICODE}

procedure TQMsgPack.BytesFromFile(AFileName: QStringW);
var
  AStream: TMemoryStream;
begin
AStream := TMemoryStream.Create;
try
  AStream.LoadFromFile(AFileName);
  BytesFromStream(AStream, 0);
finally
  FreeObject(AStream);
end;
end;

procedure TQMsgPack.BytesFromStream(AStream: TStream; ACount: Integer);
begin
DataType := mptBinary;
if ACount = 0 then
  begin
  ACount := AStream.Size;
  AStream.Position := 0;
  end
else
  begin
  if AStream.Size - AStream.Position < ACount then
    ACount := AStream.Size - AStream.Position;
  end;
SetLength(FValue, ACount);
AStream.ReadBuffer(FValue[0], ACount);
end;

procedure TQMsgPack.BytesToFile(AFileName: QStringW);
var
  AStream: TMemoryStream;
begin
AStream := TMemoryStream.Create;
try
  BytesToStream(AStream);
finally
  FreeObject(AStream);
end;
end;

procedure TQMsgPack.BytesToStream(AStream: TStream);
begin
AStream.WriteBuffer(FValue[0], Length(FValue));
end;

function TQMsgPack.GetAsBoolean: Boolean;
begin
if DataType = mptBoolean then
  Result := PBoolean(FValue)^
else if DataType = mptString then
  begin
  if not TryStrToBool(AsString, Result) then
    raise Exception.Create(Format(SBadConvert, [AsString, 'Boolean']));
  end
else if DataType in [mptFloat, mptDateTime] then
  Result := not SameValue(AsFloat, 0)
else if DataType = mptSingle then
  Result := not SameValue(AsSingle, 0)
else if DataType = mptInteger then
  Result := AsInt64 <> 0
else if DataType in [mptNull, mptUnknown] then
  Result := False
else
  raise Exception.Create(Format(SBadConvert,
    [MsgPackTypeName[Integer(DataType)], 'Boolean']));
end;

function TQMsgPack.GetAsBytes: TBytes;
begin
Result := FValue;
end;

function TQMsgPack.GetAsDateTime: TDateTime;
begin
if DataType in [mptDateTime, mptFloat] then
  Result := PDouble(FValue)^
else if DataType = mptSingle then
  Result := PSingle(FValue)^
else if DataType = mptString then
  begin
  if not(ParseDateTime(PWideChar(FValue), Result) or
    ParseWebTime(PQCharW(FValue), Result)) then
    raise Exception.Create(Format(SBadConvert,
      [MsgPackTypeName[Integer(DataType)], 'DateTime']))
  end
else if DataType in [mptInteger, mptNull, mptUnknown] then
  Result := AsInt64
else
  raise Exception.Create(Format(SBadConvert,
    [MsgPackTypeName[Integer(DataType)], 'DateTime']));
end;

function TQMsgPack.GetAsExtBytes: TBytes;
begin
if DataType = mptExtended then
  Result := FValue
else
  SetLength(Result, 0);
end;

function TQMsgPack.GetAsFloat: Double;
begin
if DataType in [mptFloat, mptDateTime] then
  Result := PDouble(FValue)^
else if DataType = mptSingle then
  Result := PSingle(FValue)^
else if DataType = mptBoolean then
  Result := Integer(AsBoolean)
else if DataType = mptString then
  begin
  if not TryStrToFloat(AsString, Result) then
    raise Exception.Create(Format(SBadConvert, [AsString, 'Numeric']));
  end
else if DataType = mptInteger then
  Result := AsInt64
else if DataType in [mptNull, mptUnknown] then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert,
    [MsgPackTypeName[Integer(DataType)], 'Numeric']))
end;

function TQMsgPack.GetAsInt64: Int64;
begin
if DataType = mptInteger then
  Result := PInt64(FValue)^
else if DataType in [mptFloat, mptDateTime] then
  Result := Trunc(PDouble(FValue)^)
else if DataType = mptSingle then
  Result := Trunc(PSingle(FValue)^)
else if DataType = mptBoolean then
  Result := Integer(AsBoolean)
else if DataType = mptString then
  Result := Trunc(AsFloat)
else if DataType in [mptNull, mptUnknown] then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert,
    [MsgPackTypeName[Integer(DataType)], 'Numeric']))
end;

function TQMsgPack.GetAsInteger: Integer;
begin
Result := AsInt64;
end;

function TQMsgPack.GetAsMsgPack: TBytes;
begin
Result := Encode;
end;

function TQMsgPack.GetAsSingle: Single;
begin
if DataType = mptSingle then
  Result := PSingle(FValue)^
else if DataType in [mptFloat, mptDateTime] then
  Result := PDouble(FValue)^
else if DataType = mptBoolean then
  Result := Integer(AsBoolean)
else if DataType = mptString then
  begin
  if not TryStrToFloat(AsString, Result) then
    raise Exception.Create(Format(SBadConvert, [AsString, 'Numeric']));
  end
else if DataType = mptInteger then
  Result := AsInt64
else if DataType in [mptNull, mptUnknown] then
  Result := 0
else
  raise Exception.Create(Format(SBadConvert,
    [MsgPackTypeName[Integer(DataType)], 'Numeric']))
end;

function TQMsgPack.GetAsString: QStringW;
  function EncodeDateTime: QStringW;
  var
    AValue: TDateTime;
  begin
  AValue := AsDateTime;
  if SameValue(AValue - Trunc(AValue), 0) then // Date
    Result := FormatDateTime(MsgPackDateFormat, AValue)
  else
    begin
    if Trunc(AValue) = 0 then
      Result := FormatDateTime(MsgPackTimeFormat, AValue)
    else
      Result := FormatDateTime(MsgPackDateTimeFormat, AValue);
    end;
  end;
  function EncodeArray: QStringW;
  const
    ArrayStart: PWideChar = '[';
    ArrayEnd: PWideChar = ']';
    ArrayDelim: PWideChar = ',';
  var
    I: Integer;
    ABuilder: TQStringCatHelperW;
  begin
  ABuilder := TQStringCatHelperW.Create;
  try
    ABuilder.Cat(ArrayStart, 1);
    if Count > 0 then
      begin
      for I := 0 to Count - 1 do
        ABuilder.Cat(Items[I].AsString).Cat(ArrayDelim, 1);
      ABuilder.Back(1);
      end;
    ABuilder.Cat(ArrayEnd, 1);
    Result := ABuilder.Value;
  finally
    FreeObject(ABuilder);
  end;
  end;
  function EncodeMap: QStringW;
  const
    MapStart: PWideChar = '{';
    MapEnd: PWideChar = '}';
    MapDelim: PWideChar = ',';
    MapValueDelim: PWideChar = ':';
    MapEmptyName: PWideChar = '""';
    MapStrStart: PWideChar = '"';
  var
    I: Integer;
    ABuilder: TQStringCatHelperW;
    AItem: TQMsgPack;
  begin
  ABuilder := TQStringCatHelperW.Create;
  try
    ABuilder.Cat(MapStart, 1);
    if Count > 0 then
      begin
      for I := 0 to Count - 1 do
        begin
        AItem := Items[I];
        if Length(AItem.Name) > 0 then
          ABuilder.Cat(QuotedStrW(AItem.Name, '"')).Cat(MapValueDelim, 1)
        else
          ABuilder.Cat(MapEmptyName).Cat(MapValueDelim, 1);
        case AItem.DataType of
          mptString, mptBinary, mptDateTime:
            ABuilder.Cat(QuotedStrW(AItem.AsString, '"'))
        else
          ABuilder.Cat(AItem.AsString);
        end;
        ABuilder.Cat(MapDelim, 1);
        end;
      ABuilder.Back(1);
      end;
    ABuilder.Cat(MapEnd, 1);
    Result := ABuilder.Value;
  finally
    FreeObject(ABuilder);
  end;
  end;
  function EncodeExtended: QStringW;
  begin
  Result := '{TypeId:' + IntToStr(FExtType) + ',Data:"' +
    qstring.BinToHex(@FValue[0], Length(FValue)) + '"';
  end;

begin
if DataType = mptString then
  Result := StrDupX(@FValue[0], Length(FValue) shr 1)
else
  begin
  case DataType of
    mptUnknown, mptNull:
      Result := '';
    mptInteger:
      Result := IntToStr(AsInt64);
    mptBoolean:
      Result := BoolToStr(AsBoolean, True);
    mptFloat:
      Result := FloatToStrF(AsFloat, ffGeneral, 15, 0);
    mptSingle:
      Result := FloatToStrF(AsSingle, ffGeneral, 7, 0);
    mptBinary:
      Result := qstring.BinToHex(@FValue[0], Length(FValue));
    mptDateTime:
      Result := EncodeDateTime;
    mptArray:
      Result := EncodeArray;
    mptMap:
      Result := EncodeMap;
    mptExtended:
      Result := EncodeExtended;
  end;
  end;
end;

function TQMsgPack.GetAsVariant: Variant;
var
  I: Integer;
  procedure BytesAsVariant;
  var
    L: Integer;
    p:PByte;
  begin
  L := Length(FValue);
  Result := VarArrayCreate([0, L - 1], varByte);
  p:=VarArrayLock(Result);
  Move(FValue[0],p^,L);
  VarArrayUnlock(Result);
  end;

begin
case DataType of
  mptString:
    Result := AsString;
  mptInteger:
    Result := AsInt64;
  mptFloat:
    Result := AsFloat;
  mptSingle:
    Result := AsSingle;
  mptDateTime:
    Result := AsDateTime;
  mptBoolean:
    Result := AsBoolean;
  mptArray, mptMap:
    begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := Items[I].AsVariant;
    end;
  mptBinary:
    BytesAsVariant;
  mptExtended:
    raise Exception.Create(SVariantNotSupport)
else
  VarClear(Result);
end;
end;

function TQMsgPack.GetCount: Integer;
begin
if DataType in [mptArray, mptMap] then
  Result := FItems.Count
else
  Result := 0;
end;

function TQMsgPack.GetEnumerator: TQMsgPackEnumerator;
begin
Result := TQMsgPackEnumerator.Create(Self);
end;

function TQMsgPack.GetIsArray: Boolean;
begin
Result := (DataType = mptArray);
end;

function TQMsgPack.GetIsDateTime: Boolean;
var
  ATime: TDateTime;
begin
Result := (DataType = mptDateTime);
if not Result then
  begin
  if DataType = mptString then
    Result := ParseDateTime(PQCharW(FValue), ATime) or
      ParseWebTime(PQCharW(FValue), ATime)
  end;
end;

function TQMsgPack.GetIsNull: Boolean;
begin
Result := (DataType = mptNull);
end;

function TQMsgPack.GetIsNumeric: Boolean;
var
  ANum: Extended;
  s: QStringW;
  p: PWideChar;
begin
Result := (DataType in [mptInteger, mptSingle, mptFloat]);
if DataType = mptString then
  begin
  s := AsString;
  p := PWideChar(s);
  if ParseNumeric(p, ANum) then
    begin
    SkipSpaceW(p);
    Result := (p^ = #0);
    end;
  end;
end;

function TQMsgPack.GetIsObject: Boolean;
begin
Result := (DataType = mptMap);
end;

function TQMsgPack.GetIsString: Boolean;
begin
Result := (DataType = mptString);
end;

function TQMsgPack.GetItemIndex: Integer;
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

function TQMsgPack.GetItems(AIndex: Integer): TQMsgPack;
begin
Result := FItems[AIndex];
end;

function TQMsgPack.GetPath: QStringW;
var
  AParent, AItem: TQMsgPack;
begin
AParent := FParent;
AItem := Self;
SetLength(Result, 0);
repeat
  if Assigned(AParent) and AParent.IsArray then
    Result := '[' + IntToStr(AItem.ItemIndex) + ']' + Result
  else if AItem.IsArray then
    Result := QMsgPackPathDelimiter + AItem.FName + Result
  else
    Result := QMsgPackPathDelimiter + AItem.FName + Result;
  AItem := AParent;
  AParent := AItem.Parent;
until AParent = nil;
if Length(Result) > 0 then
  Result := StrDupX(PQCharW(Result) + 1, Length(Result) - 1);
end;

function TQMsgPack.GetValue: QStringW;
begin
Result := AsString;
end;

function TQMsgPack.IndexOf(const AName: QStringW): Integer;
var
  I, l: Integer;
  AItem: TQMsgPack;
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
    if MsgPackCaseSensitive then
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
    else if StartWithW(PQCharW(AItem.FName), PQCharW(AName), True) then
      begin
      Result := I;
      Break;
      end;
    end;
  end;
end;

procedure TQMsgPack.InternalParse(var p: PByte; l: Integer);
var
  ps: PByte;
  I: Integer;
  ACount: Cardinal;
  AChild: TQMsgPack;
  function ParseName: QStringW;
  var
    C: Integer;
  begin
  if p^ in [$A0 .. $BF] then // 短字符串，最多31个字节
    begin
    C := p^ - $A0;
    Inc(p);
    if C > 0 then
      begin
      Result := qstring.Utf8Decode(PQCharA(p), C);
      Inc(p, C);
      end
    else
      Result := '';
    end
  else if p^ = $D9 then // Str
    begin
    Inc(p);
    C := p^;
    Inc(p);
    Result := Utf8Decode(PQCharA(p), C);
    Inc(p, C);
    end
  else if p^ = $DA then // Str16
    begin
    Inc(p);
    C := ExchangeByteOrder(PWord(p)^);
    Inc(p,2);
    Result := Utf8Decode(PQCharA(p), C);
    Inc(p, C);
    end
  else if p^ = $DB then // Str 32
    begin
    Inc(p);
    C := ExchangeByteOrder(PCardinal(p)^);
    Inc(p,4);
    Result := Utf8Decode(PQCharA(p), C);
    Inc(p, C);
    end
  else
    raise Exception.Create(SMapNameMissed);
  end;

begin
ps := p;
while IntPtr(p) - IntPtr(ps) < l do
  begin
  if p^ in [$0 .. $7F] then // 0-127的整数
    begin
    AsInteger := p^;
    Inc(p);
    Break;
    end
  else if p^ in [$80 .. $8F] then // 短映射，最多15项
    begin
    DataType := mptMap;
    FItems.Capacity := p^ - $80; // 项数
    ACount := FItems.Capacity;
    Inc(p);
    for I := 0 to ACount - 1 do
      begin
      AChild := Add;
      AChild.FName := ParseName;
      AChild.InternalParse(p, l - (Integer(p) - Integer(ps)));
      end;
    Break;
    end
  else if p^ in [$90 .. $9F] then // 短数组，最多15项
    begin
    DataType := mptArray;
    FItems.Capacity := p^ - $90;
    ACount := FItems.Capacity;
    Inc(p);
    for I := 0 to ACount - 1 do
      Add.InternalParse(p, l - (Integer(p) - Integer(ps)));
    Break;
    end
  else if p^ in [$A0 .. $BF] then // 短字符串，最多31个字节
    begin
    DataType := mptString;
    ACount := p^ - $A0;
    Inc(p);
    if ACount > 0 then
      begin
      AsString := qstring.Utf8Decode(PQCharA(p), ACount);
      Inc(p, ACount);
      end
    else
      AsString := '';
    Break;
    end
  else if p^ in [$E0 .. $FF] then
    begin
    AsInt64 := Shortint(p^);
    Inc(p);
    Break;
    end
  else
    begin
    case p^ of
      $C0: // nil/null
        begin
        DataType := mptNull;
        Inc(p);
        end;
      $C1: // 保留
        raise Exception.Create('保留的类型');
      $C2: // False
        begin
        AsBoolean := False;
        Inc(p);
        end;
      $C3: // True
        begin
        AsBoolean := True;
        Inc(p);
        end;
      $C4: // 短二进制，最长255字节
        begin
        Inc(p);
        DataType := mptBinary;
        ACount := p^;
        SetLength(FValue, ACount);
        Inc(p);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $C5: // 二进制，16位，最长65535B
        begin
        Inc(p);
        DataType := mptBinary;
        ACount := ExchangeByteOrder(PWord(p)^);
        Inc(p,2);
        SetLength(FValue, ACount);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $C6: // 二进制，32位，最长2^32-1
        begin
        Inc(p);
        DataType := mptBinary;
        ACount := ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        SetLength(FValue, ACount);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $C7: // Ext8
        begin
        Inc(p);
        DataType := mptExtended;
        ACount := p^;
        SetLength(FValue, ACount);
        Inc(p);
        FExtType := p^;
        Inc(p);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $C8: // Ext16
        begin
        Inc(p);
        DataType := mptExtended;
        ACount := ExchangeByteOrder(PWord(p)^);
        Inc(p,2);
        SetLength(FValue, ACount);
        FExtType := p^;
        Inc(p);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $C9: // Ext32,4B
        begin
        Inc(p);
        DataType := mptExtended;
        ACount := ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        SetLength(FValue, ACount);
        FExtType := p^;
        Inc(p);
        Move(p^, FValue[0], ACount);
        Inc(p, ACount);
        end;
      $CA: // float 32
        begin
        Inc(p);
        AsSingle := ExchangeByteOrder(PSingle(p)^);
        Inc(p,4);
        end;
      $CB: // Float 64
        begin
        Inc(p);
        AsFloat := ExchangeByteOrder(PDouble(p)^);
        Inc(p,8);
        end;
      $CC: // UInt8
        begin
        Inc(p);
        AsInt64 := p^;
        Inc(p);
        end;
      $CD: // UInt16
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PWord(p)^);
        Inc(p,2);
        end;
      $CE://UInt32
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        end;
      $CF://UInt64
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PInt64(p)^);
        Inc(p,8);
        end;
      $D0: // Int8
        begin
        Inc(p);
        AsInt64 := p^;
        Inc(p);
        end;
      $D1: // Int16
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PSmallint(p)^);
        Inc(p,2);
        end;
      $D2: // Int32
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PInteger(p)^);
        Inc(p,4);
        end;
      $D3: // Int64
        begin
        Inc(p);
        AsInt64 := ExchangeByteOrder(PInt64(p)^);
        Inc(p,8);
        end;
      $D4: // Fixed ext8,1B
        begin
        Inc(p);
        DataType := mptExtended;
        SetLength(FValue, 1);
        FExtType := p^;
        Inc(p);
        FValue[0] := p^;
        Inc(p);
        end;
      $D5: // Fixed Ext16,2B
        begin
        Inc(p);
        DataType := mptExtended;
        SetLength(FValue, 2);
        FExtType := p^;
        Inc(p);
        PWord(@FValue[0])^ := PWord(p)^;
        Inc(p, 2);
        end;
      $D6: // Fixed Ext32,4B
        begin
        Inc(p);
        DataType := mptExtended;
        SetLength(FValue, 4);
        FExtType := p^;
        Inc(p);
        PCardinal(@FValue[0])^ := PCardinal(p)^;
        Inc(p, 4);
        end;
      $D7: // Fixed Ext64,8B
        begin
        Inc(p);
        DataType := mptExtended;
        SetLength(FValue, 8);
        FExtType := p^;
        Inc(p);
        PInt64(@FValue[0])^ := PInt64(p)^;
        Inc(p, 8);
        end;
      $D8: // Fixed Ext 128bit,16B
        begin
        Inc(p);
        DataType := mptExtended;
        SetLength(FValue, 16);
        FExtType := p^;
        Inc(p);
        PInt64(@FValue[0])^ := PInt64(p)^;
        Inc(p, 8);
        PInt64(@FValue[8])^ := PInt64(p)^;
        Inc(p, 8);
        end;
      $D9: // Str
        begin
        Inc(p);
        ACount := p^;
        Inc(p);
        AsString := Utf8Decode(PQCharA(p), ACount);
        Inc(p, ACount);
        end;
      $DA: // Str 16
        begin
        Inc(p);
        ACount :=ExchangeByteOrder(PWord(p)^);
        Inc(p,2);
        AsString := Utf8Decode(PQCharA(p), ACount);
        Inc(p, ACount);
        end;
      $DB: // Str 32
        begin
        Inc(p);
        ACount :=ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        AsString := Utf8Decode(PQCharA(p), ACount);
        Inc(p, ACount);
        end;
      $DC: // array 16
        begin
        Inc(p);
        DataType := mptArray;
        ACount := ExchangeByteOrder(PWord(p)^);
        FItems.Capacity := ACount;
        for I := 0 to ACount - 1 do
          Add.InternalParse(p, l - (Integer(p) - Integer(ps)));
        end;
      $DD: // Array 32
        begin
        Inc(p);
        DataType := mptArray;
        ACount := ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        FItems.Capacity := ACount;
        for I := 0 to ACount - 1 do
          Add.InternalParse(p, l - (Integer(p) - Integer(ps)));
        end;
      $DE: // Object map 16
        begin
        Inc(p);
        DataType := mptMap;
        ACount := ExchangeByteOrder(PWord(p)^);
        Inc(p,2);
        FItems.Capacity := ACount;
        for I := 0 to ACount - 1 do
          begin
          AChild := Add;
          AChild.FName := ParseName;
          // 解析值
          AChild.InternalParse(p, l - (Integer(p) - Integer(ps)));
          end;
        end;
      $DF: //Object map 32
        begin
        Inc(p);
        DataType := mptMap;
        ACount := ExchangeByteOrder(PCardinal(p)^);
        Inc(p,4);
        FItems.Capacity := ACount;
        for I := 0 to ACount - 1 do
          begin
          AChild := Add;
          AChild.FName := ParseName;
          // 解析值
          AChild.InternalParse(p, l - (Integer(p) - Integer(ps)));
          end;
        end;
    end;
    end;
  Break;
  end;
end;

{$IFDEF UNICODE}

function TQMsgPack.Invoke(AInstance: TValue): TValue;
var
  AMethods: TArray<TRttiMethod>;
  AParams: TArray<TRttiParameter>;
  AMethod: TRttiMethod;
  AType: TRttiType;
  AContext: TRttiContext;
  AParamValues: array of TValue;
  I, C: Integer;
  AParamItem: TQMsgPack;
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
C := Count;
for AMethod in AMethods do
  begin
  AParams := AMethod.GetParameters;
  if Length(AParams) = C then
    begin
    SetLength(AParamValues, C);
    for I := 0 to C - 1 do
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
{$ENDIF UNICODE}

function TQMsgPack.IsChildOf(AParent: TQMsgPack): Boolean;
begin
if Assigned(AParent) then
  begin
  if AParent = FParent then
    Result := True
  else
    Result := FParent.IsChildOf(AParent);
  end
else
  Result := False;
end;

function TQMsgPack.IsParentOf(AChild: TQMsgPack): Boolean;
begin
if Assigned(AChild) then
  Result := AChild.IsChildOf(Self)
else
  Result := False;
end;

function TQMsgPack.ItemByName(AName: QStringW): TQMsgPack;
var
  AChild: TQMsgPack;
  I: Integer;
  ASelfName: String;
  function ArrayName: String;
  var
    ANamedItem: TQMsgPack;
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
if DataType = mptMap then
  begin
  I := IndexOf(AName);
  if I <> -1 then
    Result := Items[I];
  end
else if DataType = mptArray then
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

function TQMsgPack.ItemByName(const AName: QStringW; AList: TQMsgPackList;
  ANest: Boolean): Integer;
var
  ANode: TQMsgPack;
  function InternalFind(AParent: TQMsgPack): Integer;
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

function TQMsgPack.ItemByPath(APath: QStringW): TQMsgPack;
var
  AParent: TQMsgPack;
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
          if Result.DataType <> mptArray then
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

function TQMsgPack.ItemByRegex(const ARegex: QStringW; AList: TQMsgPackList;
  ANest: Boolean): Integer;
var
  ANode: TQMsgPack;
  APcre: TPerlRegEx;
  function InternalFind(AParent: TQMsgPack): Integer;
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

procedure TQMsgPack.LoadFromFile(AFileName: String);
var
  AStream: TFileStream;
begin
AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
try
  LoadFromStream(AStream);
finally
  FreeObject(AStream);
end;
end;

procedure TQMsgPack.LoadFromStream(AStream: TStream);
var
  ABytes: TBytes;
begin
SetLength(ABytes, AStream.Size - AStream.Position);
AStream.ReadBuffer(ABytes[0], Length(ABytes));
Parse(ABytes);
end;

procedure TQMsgPack.Parse(const s: TBytes);
begin
Parse(@s[0], Length(s));
end;

procedure TQMsgPack.Parse(p: PByte; l: Integer);
begin
Clear;
InternalParse(p, l);
end;

procedure TQMsgPack.Replace(AIndex: Integer; ANewItem: TQMsgPack);
begin
FreeObject(Items[AIndex]);
FItems[AIndex] := ANewItem;
end;

procedure TQMsgPack.ResetNull;
begin
DataType := mptNull;
end;

procedure TQMsgPack.SaveToFile(AFileName: String);
var
  AStream: TMemoryStream;
begin
AStream := TMemoryStream.Create;
try
  SaveToStream(AStream);
  AStream.SaveToFile(AFileName);
finally
  FreeObject(AStream);
end;
end;

procedure TQMsgPack.SaveToStream(AStream: TStream);
var
  ABytes: TBytes;
begin
ABytes := Encode;
AStream.WriteBuffer(ABytes[0], Length(ABytes));
end;

procedure TQMsgPack.SetAsBoolean(const Value: Boolean);
begin
DataType := mptBoolean;
FValue[0] := Integer(Value);
end;

procedure TQMsgPack.SetAsBytes(const Value: TBytes);
begin
DataType := mptBinary;
FValue := Value;
end;

procedure TQMsgPack.SetAsDateTime(const Value: TDateTime);
begin
DataType := mptDateTime;
PDouble(@FValue[0])^ := Value;
end;

procedure TQMsgPack.SetAsFloat(const Value: Double);
begin
DataType := mptFloat;
PDouble(@FValue[0])^ := Value;
end;

procedure TQMsgPack.SetAsInt64(const Value: Int64);
begin
DataType := mptInteger;
PInt64(@FValue[0])^ := Value;
end;

procedure TQMsgPack.SetAsInteger(const Value: Integer);
begin
SetAsInt64(Value);
end;

procedure TQMsgPack.SetAsMsgPack(const Value: TBytes);
begin
Parse(@Value[0], Length(Value));
end;

procedure TQMsgPack.SetAsSingle(const Value: Single);
begin
DataType := mptSingle;
PSingle(FValue)^ := Value;
end;

procedure TQMsgPack.SetAsString(const Value: QStringW);
var
  l: NativeInt;
begin
DataType := mptString;
l := Length(Value) shl 1;
SetLength(FValue, l);
if l > 0 then
  Move(PQCharW(Value)^, FValue[0], l);
end;

procedure TQMsgPack.SetAsVariant(const Value: Variant);
var
  I: Integer;
  AType: TVarType;
  procedure VarAsBytes;
  var
    L: Integer;
    p: PByte;
  begin
  DataType := mptBinary;
  L := VarArrayHighBound(Value, 1) + 1;
  SetLength(FValue, L);
  p := VarArrayLock(Value);
  Move(p^, FValue[0], L);
  VarArrayUnlock(Value);
  end;

begin
if VarIsArray(Value) then
  begin
  AType := VarType(Value);
  if (AType and varTypeMask) = varByte then
    VarAsBytes
  else
    begin
    ArrayNeeded(mptArray);
    Clear;
    for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
      to VarArrayHighBound(Value, VarArrayDimCount(Value)) do
      Add.AsVariant := Value[I];
    end;
  end
else
  begin
  case VarType(Value) of
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

procedure TQMsgPack.SetDataType(const Value: TQMsgPackType);
begin
if FDataType <> Value then
  begin
  if Value in [mptArray, mptMap] then
    begin
    if not Assigned(FItems) then
      FItems := TQMsgPackList.Create
    else if FDataType in [mptArray,mptMap] then
      Clear;
    end
  else
    begin
    if Assigned(FItems) then
      FreeAndNil(FItems);
    case Value of
      mptUnknown, mptNull, mptString, mptBinary, mptExtended:
        SetLength(FValue, 0);
      mptInteger:
        SetLength(FValue, SizeOf(Int64));
      mptBoolean:
        SetLength(FValue, 1);
      mptSingle:
        SetLength(FValue, SizeOf(Single));
      mptFloat, mptDateTime:
        SetLength(FValue, SizeOf(Extended));
    end;
    end;
  FDataType := Value;
  end;
end;

procedure TQMsgPack.SetExtBytes(const Value: TBytes);
begin
DataType := mptExtended;
FValue := Value;
end;

procedure TQMsgPack.SetExtType(const Value: Shortint);
begin
if FExtType <> Value then
  begin
  if Value < 0 then
    raise Exception.Create(SReservedExtType);
  FExtType := Value;
  end;
end;
{$IFDEF UNICODE}

procedure TQMsgPack.ToRecord<T>(var ARecord: T);
begin
ToRtti(@ARecord, TypeInfo(T));
end;

procedure TQMsgPack.ToRtti(ADest: Pointer; AType: PTypeInfo);
  function MsgPackToValueArray(AMsgPack: TQMsgPack): TValueArray;
  var
    I: Integer;
    AChild: TQMsgPack;
  begin
  SetLength(Result, AMsgPack.Count);
  for I := 0 to AMsgPack.Count - 1 do
    begin
    AChild := AMsgPack[I];
    case AChild.DataType of
      mptNull:
        Result[I] := TValue.Empty;
      mptString:
        Result[I] := AChild.AsString;
      mptInteger:
        Result[I] := AChild.AsInteger;
      mptSingle, mptFloat:
        Result[I] := AChild.AsFloat;
      mptBoolean:
        Result[I] := AChild.AsBoolean;
      mptDateTime:
        Result[I] := AChild.AsDateTime;
      mptArray, mptMap:
        Result[I] := TValue.FromArray(TypeInfo(TValue),
          MsgPackToValueArray(AChild));
      mptBinary, mptExtended:
        raise Exception.Create(SVariantNotSupport);
    end;
    end;
  end;

  procedure LoadCollection(AMsgPack: TQMsgPack; ACollection: TCollection);
  var
    I: Integer;
  begin
  for I := 0 to AMsgPack.Count - 1 do
    AMsgPack.ToRtti(ACollection.Add);
  end;
  procedure ToRecord;
  var
    AContext: TRttiContext;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    ABaseAddr: Pointer;
    J: Integer;
    AChild: TQMsgPack;
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
                  if AChild.DataType = mptInteger then
                    PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otUByte:
                  begin
                  if AChild.DataType = mptInteger then
                    PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otSWord:
                  begin
                  if AChild.DataType = mptInteger then
                    PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otUWord:
                  begin
                  if AChild.DataType = mptInteger then
                    PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otSLong:
                  begin
                  if AChild.DataType = mptInteger then
                    PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      AChild.AsInteger
                  else
                    PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                      GetEnumValue(AFields[J].FieldType.Handle,
                      AChild.AsString);
                  end;
                otULong:
                  begin
                  if AChild.DataType = mptInteger then
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
                if AChild.DataType = mptInteger then
                  PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PShortint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otUByte:
                begin
                if AChild.DataType = mptInteger then
                  PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PByte(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otSWord:
                begin
                if AChild.DataType = mptInteger then
                  PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PSmallint(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otUWord:
                begin
                if AChild.DataType = mptInteger then
                  PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PWord(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otSLong:
                begin
                if AChild.DataType = mptInteger then
                  PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    AChild.AsInteger
                else
                  PInteger(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
                    StringToSet(AFields[J].FieldType.Handle, AChild.AsString);
                end;
              otULong:
                begin
                if AChild.DataType = mptInteger then
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
            begin
            if (AFields[J].FieldType.Handle = TypeInfo(TDateTime)) or
              (AFields[J].FieldType.Handle = TypeInfo(TTime)) or
              (AFields[J].FieldType.Handle = TypeInfo(TDate)) then
              AFields[J].SetValue(ABaseAddr, AChild.AsDateTime)
            else
              AFields[J].SetValue(ABaseAddr, AChild.AsFloat);
            end;
          tkInt64:
            AFields[J].SetValue(ABaseAddr, AChild.AsInt64);
          tkVariant:
            PVariant(IntPtr(ABaseAddr) + AFields[J].Offset)^ :=
              AChild.AsVariant;
          tkArray, tkDynArray:
            AFields[J].SetValue(ABaseAddr,
              TValue.FromArray(AFields[J].FieldType.Handle,
              MsgPackToValueArray(AChild)));
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
    AChild: TQMsgPack;
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
        tkRecord, tkArray, tkDynArray:
          // tkArray,tkDynArray类型的属性没见过,tkRecord存疑
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
          else if AChild.DataType = mptInteger then
            SetOrdProp(AObj, AProp, AChild.AsInteger)
          else
            SetEnumProp(AObj, AProp, AChild.AsString);
          end;
        tkSet:
          if AChild.DataType = mptInteger then
            SetOrdProp(AObj, AProp, AChild.AsInteger)
          else
            SetSetProp(AObj, AProp, AChild.AsString);
        tkVariant:
          SetVariantProp(AObj, AProp, AChild.AsVariant);
        tkInt64:
          SetInt64Prop(AObj, AProp, AChild.AsInt64);
      end;
      end;
    end;
  end;
  function ArrayItemTypeName(ATypeName: QStringW): QStringW;
  var
    p, ps: PQCharW;
    ACount: Integer;
  begin
  p := PQCharW(ATypeName);
  if StartWithW(p, 'TArray<', True) then
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
    AChild: TQMsgPack;
  begin
  AContext := TRttiContext.Create;
{$IF RTLVersion>25}
  s := ArrayItemTypeName(AType.NameFld.ToString);
{$ELSE}
  s := ArrayItemTypeName(String(AType.Name));
{$IFEND}
  ASubType := AContext.FindType(s);
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
          case GetTypeData(ASubType.Handle).OrdType of
            otSByte:
              PShortint(pi)^ := AChild.AsInteger;
            otUByte:
              pi^ := AChild.AsInteger;
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
          if GetTypeData(ASubType.Handle)^.BaseType^ = TypeInfo(Boolean) then
            PBoolean(pi)^ := AChild.AsBoolean
          else
            begin
            case GetTypeData(ASubType.Handle)^.OrdType of
              otSByte:
                begin
                if AChild.DataType = mptInteger then
                  PShortint(pi)^ := AChild.AsInt64
                else
                  PShortint(pi)^ := GetEnumValue(ASubType.Handle,
                    AChild.AsString);
                end;
              otUByte:
                begin
                if AChild.DataType = mptInteger then
                  pi^ := AChild.AsInt64
                else
                  pi^ := GetEnumValue(ASubType.Handle, AChild.AsString);
                end;
              otSWord:
                begin
                if AChild.DataType = mptInteger then
                  PSmallint(pi)^ := AChild.AsInt64
                else
                  PSmallint(pi)^ := GetEnumValue(ASubType.Handle,
                    AChild.AsString);
                end;
              otUWord:
                begin
                if AChild.DataType = mptInteger then
                  PWord(pi)^ := AChild.AsInt64
                else
                  PWord(pi)^ := GetEnumValue(ASubType.Handle, AChild.AsString);
                end;
              otSLong:
                begin
                if AChild.DataType = mptInteger then
                  PInteger(pi)^ := AChild.AsInt64
                else
                  PInteger(pi)^ := GetEnumValue(ASubType.Handle,
                    AChild.AsString);
                end;
              otULong:
                begin
                if AChild.DataType = mptInteger then
                  PCardinal(pi)^ := AChild.AsInt64
                else
                  PCardinal(pi)^ := GetEnumValue(ASubType.Handle,
                    AChild.AsString);
                end;
            end;
            end;
          end;
        tkFloat:
          case GetTypeData(ASubType.Handle)^.FloatType of
            ftSingle:
              PSingle(pi)^ := AChild.AsFloat;
            ftDouble:
              PDouble(pi)^ := AChild.AsFloat;
            ftExtended:
              PExtended(pi)^ := AChild.AsFloat;
            ftComp:
              PComp(pi)^ := AChild.AsFloat;
            ftCurr:
              PCurrency(pi)^ := AChild.AsFloat;
          end;
{$IFNDEF NEXTGEN}
        tkString:
          PShortString(pi)^ := ShortString(AChild.AsString);
{$ENDIF !NEXTGEN}
        tkSet:
          begin
          case GetTypeData(ASubType.Handle)^.OrdType of
            otSByte:
              PShortint(pi)^ := StringToSet(ASubType.Handle, AChild.AsString);
            otUByte:
              pi^ := StringToSet(ASubType.Handle, AChild.AsString);
            otSWord:
              PSmallint(pi)^ := StringToSet(ASubType.Handle, AChild.AsString);
            otUWord:
              PWord(pi)^ := StringToSet(ASubType.Handle, AChild.AsString);
            otSLong:
              PInteger(pi)^ := StringToSet(ASubType.Handle, AChild.AsString);
            otULong:
              PCardinal(pi)^ := StringToSet(ASubType.Handle, AChild.AsString);
          end;
          end;
        tkClass:
          begin
          if PPointer(pi)^ <> nil then
            begin
            AChildObj := PPointer(pi)^;
            if AChildObj is TStrings then
              (AChildObj as TStrings).Text := AChild.AsString
            else if AChildObj is TCollection then
              LoadCollection(AChild, AChildObj as TCollection)
            else
              AChild.ToRtti(AChildObj);
            end;
          end;
        tkWChar:
          PWideChar(pi)^ := PWideChar(AChild.AsString)[0];
{$IFNDEF NEXTGEN}
        tkLString:
          PAnsiString(pi)^ := AnsiString(AChild.AsString);
        tkWString:
          PWideString(pi)^ := AChild.AsString;
{$ENDIF}
        tkVariant:
          PVariant(pi)^ := AChild.AsVariant;
        tkArray, tkDynArray:
          AChild.ToRtti(pi, ASubType.Handle);
        tkRecord:
          AChild.ToRtti(pi, ASubType.Handle);
        tkInt64:
          PInt64(pi)^ := AChild.AsInt64;
        tkUString:
          PUnicodeString(pi)^ := AChild.AsString;
      end;
      end;
    end
  else
    raise Exception.Create(SArrayTypeMissed);
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
  else
    raise Exception.Create(SUnsupportPropertyType);
  end;
end;

procedure TQMsgPack.ToRtti(AInstance: TValue);
begin
if AInstance.IsEmpty then
  Exit;
if AInstance.Kind = tkRecord then
  ToRtti(AInstance.GetReferenceToRawData, AInstance.TypeInfo)
else if AInstance.Kind = tkClass then
  ToRtti(AInstance.AsObject, AInstance.TypeInfo)
end;

function TQMsgPack.ToRttiValue: TValue;
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
  mptString:
    Result := AsString;
  mptInteger:
    Result := AsInt64;
  mptSingle:
    Result := AsSingle;
  mptFloat:
    Result := AsFloat;
  mptDateTime:
    Result := AsDateTime;
  mptBoolean:
    Result := AsBoolean;
  mptBinary, mptExtended:
    raise Exception.Create(SUnsupportValueType);
  mptArray, mptMap: // 数组和对象都只能当成数组来处理
    AsDynValueArray
else
  Result := TValue.Empty;
end;
end;
{$ENDIF UNICODE}

function TQMsgPack.ToString: string;
begin
if Length(FName) > 0 then
  Result := FName + ':' + AsString
else
  Result := AsString;
end;

function TQMsgPack.ValueByName(AName, ADefVal: QStringW): QStringW;
var
  AChild: TQMsgPack;
begin
AChild := ItemByName(AName);
if Assigned(AChild) then
  Result := AChild.AsString
else
  Result := ADefVal;
end;

function TQMsgPack.ValueByPath(APath, ADefVal: QStringW): QStringW;
var
  AItem: TQMsgPack;
begin
AItem := ItemByPath(APath);
if Assigned(AItem) then
  Result := AItem.AsString
else
  Result := ADefVal;
end;

{ TQMsgPackEnumerator }

constructor TQMsgPackEnumerator.Create(AList: TQMsgPack);
begin
inherited Create;
FList := AList;
FIndex := -1;
end;

function TQMsgPackEnumerator.GetCurrent: TQMsgPack;
begin
Result := FList[FIndex];
end;

function TQMsgPackEnumerator.MoveNext: Boolean;
begin
if FIndex < FList.Count - 1 then
  begin
  Inc(FIndex);
  Result := True;
  end
else
  Result := False;
end;

{ TQHashedMsgPack }

function TQHashedMsgPack.Add(AName: QStringW): TQMsgPack;
begin
Result := inherited Add(AName);
Result.FNameHash := HashOf(PQCharW(AName), Length(AName) shl 1);
FHashTable.Add(Pointer(Count - 1), Result.FNameHash);
end;

procedure TQHashedMsgPack.Assign(ANode: TQMsgPack);
begin
inherited;
if (Length(FName) > 0) then
  begin
  if FNameHash = 0 then
    FNameHash := HashOf(PQCharW(FName), Length(FName) shl 1);
  if Assigned(Parent) then
    TQHashedMsgPack(Parent).FHashTable.Add(Pointer(Parent.Count - 1),
      FNameHash);
  end;
end;

procedure TQHashedMsgPack.Clear;
begin
inherited;
FHashTable.Clear;
end;

constructor TQHashedMsgPack.Create;
begin
inherited;
FHashTable := TQHashTable.Create();
FHashTable.AutoSize := True;
end;

function TQHashedMsgPack.CreateItem: TQMsgPack;
begin
if Assigned(OnQMsgPackCreate) then
  Result := OnQMsgPackCreate
else
  Result := TQHashedMsgPack.Create;
end;

procedure TQHashedMsgPack.Delete(AIndex: Integer);
var
  AItem: TQMsgPack;
begin
AItem := Items[AIndex];
FHashTable.Delete(Pointer(AIndex), AItem.NameHash);
inherited;
end;

destructor TQHashedMsgPack.Destroy;
begin
FreeObject(FHashTable);
inherited;
end;

function TQHashedMsgPack.IndexOf(const AName: QStringW): Integer;
var
  AIndex, AHash: Integer;
  AList: PQHashList;
  AItem: TQMsgPack;
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

procedure TQHashedMsgPack.Replace(AIndex: Integer; ANewItem: TQMsgPack);
var
  AOld: TQMsgPack;
begin
if not(ANewItem is TQHashedMsgPack) then
  raise Exception.CreateFmt(SReplaceTypeNeed, ['TQHashedMsgPack']);
AOld := Items[AIndex];
FHashTable.Delete(Pointer(AIndex), AOld.NameHash);
inherited;
if Length(ANewItem.FName) > 0 then
  FHashTable.Add(Pointer(AIndex), ANewItem.FNameHash);
end;

initialization

MsgPackDateFormat := 'yyyy-mm-dd';
MsgPackTimeFormat := 'hh:nn:ss.zzz';
MsgPackDateTimeFormat := 'yyyy-mm-dd"T"hh:nn:ss.zzz';
MsgPackCaseSensitive := True;
MsgPackRttiEnumAsInt := True;
OnQMsgPackCreate := nil;
OnQMsgPackFree := nil;

end.
