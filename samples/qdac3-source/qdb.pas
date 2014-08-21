unit qdb;

interface

{$I 'qdac.inc'}

{ dbcommon实现了表达式过滤
}
uses classes, sysutils, qstring, qrbtree, qworker, db, fmtbcd, dbcommon,
  SqlTimSt, variants, syncobjs,qvalue
{$IFDEF UNICODE}
    , System.Generics.Collections
{$ENDIF}
    ;

const
  // 数据库架构对象定义
  SCHEMA_DATABASE = $80000000; // 数据库
  SCHEMA_SCHEMA = $40000000; // 元数据定义
  SCHEMA_TABLE = $20000000; // 数据表
  SCHEMA_COLUMN = $10000000; // 数据列
  SCHEMA_TYPE = $08000000; // 数据类型
  SCHEMA_METATYPE = $FF000000; // 元数据类型
  // 数据列属性定义
  SCHEMA_ISINDEX = $00000001; // 索引
  SCHEMA_ISPRIMARY = $00000002; // 主键
  SCHEMA_NULLABLE = $000000004; // 允许为空
  SCHEMA_ISFIXED = $00000008; // 固定长度
  SCHEMA_AUTOINC = $00000010; // 自动增加
  SCHEMA_VISIBLE = $00000020; // 列是否可见
  SCHEMA_READONLY = $00000040; // 列是否只读
  SCHEMA_UNNAMED = $00000080; // 未命名列
  SCHEMA_CALC = $00000100; // 内部计算列
  SCHEMA_ARRAY = $00000200; // 数组类型
  SCHEMA_INWHERE = $00000400; // 可在Where条件中使用，用于更新或删除数据
  SCHEMA_UNIQUE = $00000800; // 唯一约束
  SCHEMA_COLUMNATTR = $000001FF; // 列属性掩码

  // SQL数据类型(32位整数代表数据类型,高16位为掩码，低16位为类型编码)
  SQL_MASK_COMPLEX = $80000000;
  SQL_MASK_NUMERIC = $40000000; // 数值类型ID的掩码
  SQL_MASK_FIXEDSIZE = $20000000; // 固定大小
  SQL_MASK_INTEGER = SQL_MASK_NUMERIC OR SQL_MASK_FIXEDSIZE OR $10000000;
  // 是一个整数类型
  SQL_MASK_UNSIGNED = SQL_MASK_INTEGER OR $08000000; // 无符号类型
  SQL_MASK_FLOAT = SQL_MASK_NUMERIC OR SQL_MASK_FIXEDSIZE; // 浮点数
  SQL_MASK_SPEC = $04000000; // 是特定数据特有类型，如PostgreSQL的特定类型
  SQL_MASK_ARRAY = $02000000; // 是数组的一部分
  SQL_MASK_BINARY = $01000000; // 是二进制数据类型
  SQL_MASK_AUTOINC = SQL_MASK_INTEGER OR $00800000; // 是自增的序列
  SQL_MASK_CHAR = $00400000; // 字符
  SQL_MASK_TIME = SQL_MASK_FIXEDSIZE OR $00200000; // 日期时间类型
  SQL_MASK_LONGSIZE = $00100000; // 长长度类型

  // 字符
  SQL_BASE_CHAR = $00000001; // ANSI字符
  SQL_BASE_WIDECHAR = $00000002; // Unicode字符
  // 整数
  SQL_BASE_BYTE = $00000003; // 单字节
  SQL_BASE_WORD = $00000004; // 双字节
  SQL_BASE_DWORD = $00000005; // 四字节
  SQL_BASE_QWORD = $00000006; // 八字节

  // 浮点数
  SQL_BASE_SINGLE = $00000007; // 单精度浮点值
  SQL_BASE_DOUBLE = $00000008; // 双精度浮点值
  SQL_BASE_EXTENDED = $00000009; // 扩展浮点类型
  SQL_BASE_BCD = $0000000A; // BCD类型
  SQL_BASE_SMALLMONEY = $0000000B; // 短货币
  SQL_BASE_MONEY = $0000000C; // 长货币

  SQL_BASE_BOOLEAN = $0000000D; // 布尔
  SQL_BASE_UUID = $0000000E; // UUID类型
  SQL_BASE_BIT = $0000000F; // 位类型

  // 日期时间
  SQL_BASE_TIME = $00000010; // 时间类型
  SQL_BASE_DATE = $00000011; // 日期类型
  SQL_BASE_SMALLDATETIME = $00000012; // 短日期时间类型
  SQL_BASE_DATETIME = $00000013; // 日期时间类型
  SQL_BASE_INTERVAL = $00000014; // 时间间隔
  SQL_BASE_TIMEOFFSET = $00000015; // 时间偏移
  SQL_BASE_TIMESTAMP = $00000016; // 时间戳

  SQL_BASE_BINARY = $00000017; // 二进制
  // 扩展的类型
  SQL_BASE_PICTURE = $00000018; // 图片（好吧，这个实际上仅早期的少数数据库支持，实际不会用到）
  SQL_BASE_STREAM = $00000019; // 数据流
  SQL_BASE_XML = $0000001A; // XML数据
  SQL_BASE_JSON = $0000001B; // JSON数据

  SQL_BASE_OID = $0000001C; // OID
  SQL_BASE_POINT = $0000001D; // 点
  SQL_BASE_LINE = $0000001E; // 线
  SQL_BASE_LSEG = $0000001F; // 线段
  SQL_BASE_BOX = $00000020; // 矩形
  SQL_BASE_PATH = $00000021; // 路径
  SQL_BASE_POLYGON = $00000022; // 多边形
  SQL_BASE_CIRCLE = $00000023; // 圆
  SQL_BASE_CIDR = $00000024; // 可以带掩码IP地址
  SQL_BASE_INET = $00000025; // IP
  SQL_BASE_MACADDR = $00000026; // 网卡物理地址
  SQL_BASE_ROWS = $00000027; // 行集(记录集)
  SQL_BASE_ACL = $00000028; // 访问控制列表
  SQL_BASE_DATASET = $00000029; // 数据集
  SQL_BASE_CURSOR = $0000002A; // 游标
  SQL_BASE_VARIANT = $0000002B; // 变体
  SQL_BASE_INTERFACE = $0000002C; // 接口
  SQL_BASE_IDISPATCH = $0000002D; // IDispatch
  SQL_BASE_OBJECT = $0000002E; // 对象
  SQL_BASE_PARAMS = $0000002F; // 参数
  SQL_BASE_CONNECTION = $00000030; // 连接
  SQL_BASE_OLE = $00000031; // OLE对象，用OLESave和OLELoad保存和加载
  SQL_BASE_POINTER = $00000032; // 指针引用
  SQL_BASE_ENUM = $00000033; // 枚举
  SQL_BASE_SET = $00000034; // 集合
  SQL_BASE_TSVECTOR = $00000035; // 全文检索向量
  SQL_BASE_TSQUERY = $00000036; // 全文检索查询
  SQL_BASE_ADT = $00000027; // 高级数据类型，用户在服务器端定义的数据类型
  // 基本类型

  SQL_UNKNOWN = $00000000; // 未知类型

  // 整数类型
  SQL_TINYINT = SQL_MASK_INTEGER OR SQL_BASE_BYTE; // -128-127
  SQL_BYTE = SQL_TINYINT OR SQL_MASK_UNSIGNED; // 0-255
  SQL_SMALLINT = SQL_MASK_INTEGER OR SQL_BASE_WORD; // 有符号的-32768-32767
  SQL_WORD = SQL_SMALLINT OR SQL_MASK_UNSIGNED; // 无符号整数，0-65535
  SQL_INTEGER = SQL_MASK_INTEGER OR SQL_BASE_DWORD; // 有符号的32位整数
  SQL_DWORD = SQL_INTEGER OR SQL_MASK_UNSIGNED; // 无符号的32位整数
  SQL_INT64 = SQL_MASK_INTEGER OR SQL_BASE_QWORD; // 有符号的64位整数
  SQL_QWORD = SQL_INT64 OR SQL_MASK_UNSIGNED; // 无符号的64位整数
  SQL_SMALLSERIAL = SQL_SMALLINT OR SQL_MASK_AUTOINC; // 16位自增
  SQL_SERIAL = SQL_INTEGER OR SQL_MASK_AUTOINC; // 32位自增序列
  SQL_BIGSERIAL = SQL_INT64 OR SQL_MASK_AUTOINC; // 64位自增序列

  // 浮点类型
  SQL_SINGLE = SQL_MASK_FLOAT OR SQL_BASE_DWORD OR SQL_BASE_SINGLE; // 有符号的32位实数
  SQL_FLOAT = SQL_MASK_FLOAT OR SQL_BASE_QWORD OR SQL_BASE_DOUBLE; // 有符号的64位实数
  SQL_BCD = SQL_MASK_FLOAT OR SQL_BASE_BCD; // 有符号的任意精度实数
  SQL_NUMERIC = SQL_BCD;
  SQL_MONEY = SQL_MASK_FLOAT OR SQL_BASE_MONEY; // 货币类型
  SQL_SMALLMONEY = SQL_MASK_FLOAT OR SQL_BASE_SMALLMONEY; // 小货币类型
  SQL_EXTENDED = SQL_MASK_FLOAT OR SQL_BASE_EXTENDED;

  // 字符串类型
  SQL_CHAR = SQL_MASK_FIXEDSIZE OR SQL_MASK_CHAR OR SQL_BASE_CHAR; // 固定长度字符串
  SQL_VARCHAR = SQL_MASK_CHAR OR SQL_BASE_CHAR; // 变长字符串
  SQL_WIDECHAR = SQL_MASK_FIXEDSIZE OR SQL_MASK_CHAR OR SQL_BASE_WIDECHAR;
  // 固定长度Unicode字符串
  SQL_WIDEVARCHAR = SQL_MASK_CHAR OR SQL_BASE_WIDECHAR; // 变长Unicode字符串
  SQL_TEXT = SQL_VARCHAR OR SQL_MASK_LONGSIZE; // 文本
  SQL_WIDETEXT = SQL_WIDEVARCHAR OR SQL_MASK_LONGSIZE; // Unicode文本
  SQL_XML = SQL_WIDETEXT OR SQL_BASE_XML;
  SQL_JSON = SQL_WIDETEXT OR SQL_BASE_JSON;

  // 二进制数据类型
  SQL_BINARY = SQL_MASK_FIXEDSIZE OR SQL_MASK_BINARY or SQL_BASE_BINARY;
  // 二进制数据
  SQL_BYTES = SQL_BINARY;
  SQL_BIT = SQL_MASK_FIXEDSIZE OR SQL_BASE_BIT OR SQL_MASK_BINARY;
  SQL_VARBIT = SQL_BASE_BIT OR SQL_MASK_BINARY;
  SQL_VARBINARY = SQL_MASK_BINARY or SQL_BASE_BINARY; // 变长二进制数据
  SQL_VARBYTES = SQL_VARBINARY;
  SQL_LARGEOBJECT = SQL_VARBINARY OR SQL_MASK_LONGSIZE; // 大二进制对象(BLOB)
  SQL_PICTURE = SQL_LARGEOBJECT OR SQL_BASE_PICTURE; // 图片数据
  SQL_STREAM = SQL_LARGEOBJECT OR SQL_BASE_STREAM; // 流对象
  SQL_OLE = SQL_LARGEOBJECT OR SQL_BASE_OLE;

  SQL_BOOLEAN = SQL_MASK_FIXEDSIZE OR SQL_BASE_BOOLEAN; // 布尔
  SQL_UUID = SQL_MASK_FIXEDSIZE OR SQL_BASE_UUID;
  SQL_GUID = SQL_UUID;
  SQL_BITS = SQL_MASK_FIXEDSIZE OR SQL_BASE_BIT;
  SQL_VARBITS = SQL_BASE_BIT;

  // 日期时间类型
  SQL_DATE = SQL_MASK_TIME OR SQL_BASE_DATE; // 日期
  SQL_TIME = SQL_MASK_TIME OR SQL_BASE_TIME; // 时间
  SQL_SMALLDATETIME = SQL_MASK_TIME or SQL_BASE_SMALLDATETIME; // 小日期时间类型
  SQL_DATETIME = SQL_MASK_TIME OR SQL_BASE_DATETIME; // 日期时间
  SQL_TIMESTAMP = SQL_MASK_TIME OR SQL_BASE_TIMESTAMP; // 时间戳
  SQL_INTERVAL = SQL_MASK_TIME OR SQL_BASE_INTERVAL; // 时间间隔
  SQL_TIMEOFFSET = SQL_MASK_TIME OR SQL_BASE_TIMEOFFSET; // 时间偏移

  SQL_DATASET = SQL_MASK_COMPLEX OR SQL_BASE_DATASET; // 数据集
  SQL_CURSOR = SQL_MASK_COMPLEX OR SQL_BASE_CURSOR; // 游标
  SQL_VARIANT = SQL_MASK_COMPLEX OR SQL_BASE_VARIANT; // 变体
  SQL_INTERFACE = SQL_MASK_COMPLEX OR SQL_BASE_INTERFACE; // 接口
  SQL_IDISPATCH = SQL_MASK_COMPLEX OR SQL_BASE_IDISPATCH; // IDispatch
  SQL_OBJECT = SQL_MASK_COMPLEX OR SQL_BASE_OBJECT; // 对象
  SQL_PARAMS = SQL_MASK_COMPLEX OR SQL_BASE_PARAMS; // 参数
  SQL_CONNECTION = SQL_MASK_COMPLEX OR SQL_BASE_CONNECTION; // 连接
  SQL_REFERENCE = SQL_MASK_COMPLEX OR SQL_BASE_POINTER; // 指针引用，这种类型仅在运行时有效
  SQL_ARRAY = SQL_MASK_COMPLEX OR SQL_MASK_ARRAY; // 数组
  SQL_ADT = SQL_MASK_COMPLEX OR SQL_MASK_ARRAY OR SQL_BASE_ADT; // 高级数据类型

  // PostgreSQL类型
  SQL_PG_OID = SQL_MASK_SPEC OR SQL_DWORD OR SQL_MASK_AUTOINC OR SQL_BASE_OID;
  SQL_PG_POINT = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_POINT;
  SQL_PG_LINE = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_LINE;
  SQL_PG_LSEG = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_LSEG;
  SQL_PG_BOX = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_BOX;
  SQL_PG_PATH = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_PATH;
  SQL_PG_POLYGON = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_POLYGON;
  SQL_PG_CIRCLE = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_CIRCLE;
  SQL_PG_CIDR = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_CIDR;
  SQL_PG_INET = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_INET;
  SQL_PG_MACADDR = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_MACADDR;
  SQL_PG_ROWS = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_ROWS;
  SQL_PG_ACL = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_ACL;
  SQL_PG_ENUM = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_BASE_ENUM;
  SQL_PG_TSVECTOR = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_MASK_CHAR OR
    SQL_BASE_TSVECTOR;
  SQL_PG_TSQUERY = SQL_MASK_SPEC OR SQL_MASK_COMPLEX OR SQL_MASK_CHAR OR
    SQL_BASE_TSQUERY;

  // 已知错误代码
  PROV_ERROR_SQL_EMPTY = $80000001; // 脚本为空
  PROV_EEROR_RESULT_EMPTY = $80000002; // 结果集为空
  PROV_DRIVER_NOT_FOUND = $80000003; // 驱动程序对应的动态链接库未找到
  PROV_NOT_CONNECTED = $80000004; // 连接未就绪
  // Provider标志位
  PF_CONNECTING = $00000001; // 正在连接数据库
  PF_CONNECTED = $00000002; // 已经连接到数据库
  PF_CLOSING = $00000004; // 正在关闭连接
  PF_CLOSED = $00000008; // 连接已经关闭
  PF_EXECUTING = $00000010; // 连接正在执行脚本
  PF_KEEPALIVE = $00000020; // 需要进行连接保持测试
  PF_PEEKING = $00000040; // 正在执行连接保持测试

type

  TQField = class;
  TQSchema = class;
  PQDataSet = ^TQDataSet;
  TQDataSet = class;
  TQProvider = class;
  TQConverter = class;
  TQConverterClass = class of TQConverter;

  TQRecord = class;
{$IFDEF UNICODE}
  TQFieldList = TList<TQField>;

  TQSchemaList = TList<TQSchema>;
  TQDataSetList = TList<TQDataSet>;
  TQRecords = TList<TQRecord>;
{$ELSE}
  TQFieldList = TList;
  TQSchemaList = TList;

  TQDataSetList = TList;
  TQRecords = TList;
{$ENDIF}

  IQColumnValue = interface;
  IQField = interface;
  IQRecord = interface;
  IQDataProducer = interface;






  PQColumnValue = ^TQColumnValue;

  IQColumnValue = interface
    function GetChanged: Boolean;
    function GetOldValue: IQValue;
    function GetNewValue: IQValue;
    function GetCurrentValue: IQValue;
  end;

  TQColumnValue = class(TInterfacedObject, IQColumnValue)
  private
    FOld: TQValue; // 原始值
    FNew: TQValue; // 新值
    FCurrent: TQValue; // 当前值，如果Changed为True，则指向New，否则指向Old
    FField: TQField;
    function GetChanged: Boolean; // 是否发生变更
    function GetOldValue: IQValue;
    function GetNewValue: IQValue;
    function GetCurrentValue: IQValue;
  public
    constructor Create(AField: TQField); overload;
    destructor Destroy; override;
    property OldValue: TQValue read FOld;
    property NewValue: TQValue read FNew;
    property Current: TQValue read FCurrent;
    property Changed: Boolean read GetChanged;
    property Field: TQField read FField;
  end;

  TQColumnValues = array of TQColumnValue;

  IQRecord = interface
    ['{EAE71A1D-D401-447D-8119-74CC9E52FA22}']
    function GetOwner: IQDataProducer;
    function GetUpdateStatus: TUpdateStatus;
  end;

  TQRecord = class
  private
    FOriginIndex: Integer; // 在克隆时原始数据集中的索引
    FItemIndex: Integer; // 当前数据集中的记录索引
    FSortedIndex: Integer; // 当前排序结果中的记录索引
    FFilteredIndex: Integer; // 当前过滤结果中的记录索引
    FRefCount: Integer; // 引用计数
    FBookmark: Pointer; // 记录对应的书签,指向记录自己
    FValues: TQColumnValues; // 记录值列表
    FStatus: TUpdateStatus; // 记录状态
    FOwner: IQDataProducer;
    function GetOwner: IQDataProducer;
  public
    constructor Create(AOwner: IQDataProducer); overload;
    destructor Destroy; override;
    property Owner: IQDataProducer read FOwner;
    property OriginIndex: Integer read FOriginIndex; // 在克隆时原始数据集中的索引
    property ItemIndex: Integer read FItemIndex; // 当前数据集中的记录索引
    property SortedIndex: Integer read FSortedIndex; // 当前排序结果中的记录索引
    property FilteredIndex: Integer read FFilteredIndex; // 当前过滤结果中的记录索引
    property RefCount: Integer read FRefCount; // 引用计数
    property Bookmark: Pointer read FBookmark; // 记录对应的书签,指向记录自己
    property Values: TQColumnValues read FValues; // 记录值列表
    property Status: TUpdateStatus read FStatus; // 记录状态
  end;

  IQField = interface
    ['{EA1F770A-3F40-47A6-B524-C739C1DCBED1}']
    function GetCount: Integer;
    function GetItems(const AIndex: Integer): IQField;
    function AddChild: IQField;
    procedure DeleteChild(const AIndex: Integer);
    procedure Clear;
    function IndexOfName(const S: QStringW): Integer;
    function GetSchema: QStringW;
    procedure SetSchema(const S: QStringW);
    function GetTable: QStringW;
    procedure SetTable(const S: QStringW);
    function GetDatabase: QStringW;
    procedure SetDatabase(const S: QStringW);
    function GetName: QStringW;
    procedure SetName(const S: QStringW);
    function GetBaseName: QStringW;
    procedure SetBaseName(const S: QStringW);
    function GetDBType: Integer;
    procedure SetDBType(const V: Integer);
    function GetOwner: IQDataProducer;
    function GetDataType: TFieldType;
    function GetPrecision: Word;
    procedure SetPrecision(const V: Word);
    function GetScale: Word;
    procedure SetScale(const V: Word);
    function GetSize: Integer;
    procedure SetSize(const V: Integer);
    function GetDBNo: Word;
    procedure SetDBNo(const V: Word);
    function GetFieldNo: Word;
    procedure SetFieldNo(const V: Word);
    function GetParent: IQField;
    function GetIndex: Integer;
    function GetInWhere: Boolean;
    procedure SetInWhere(const V: Boolean);
    function GetIsArray: Boolean;
    procedure SetIsArray(const V: Boolean);
    function GetIsAutoInc: Boolean;
    procedure SetIsAutoInc(const V: Boolean);
    function GetIsCalc: Boolean;
    procedure SetIsCalc(const V: Boolean);
    function GetIsFixed: Boolean;
    procedure SetIsFixed(const V: Boolean);
    function GetIsIndex: Boolean;
    procedure SetIsIndex(const V: Boolean);
    function GetIsPrimary: Boolean;
    procedure SetIsPrimary(const V: Boolean);
    function GetIsUnique: Boolean;
    procedure SetIsUnique(const V: Boolean);
    function GetNullable: Boolean;
    procedure SetNullable(const V: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const V: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(const V: Boolean);
    function GetPrivateData: Pointer;
    procedure SetPrivateData(const p: Pointer; ASize: Integer);
  end;

  TQField = class(TInterfacedObject, IQField)
  private
    FSchema: QStringW; // 架构名，如public
    FDatabase: QStringW; // 数据库名
    FTable: QStringW; // 表名
    FBaseName: QStringW; // 数据表中原始的列名
    FName: QStringW; // 显示的列名
    FDBType: Integer; // 数据字段类型
    FFlags: Integer; // 标志位
    FFieldDef: TFieldDef; // 关联的字段定义
    FField: TField; // 关联的字段
    FDataSet: TQDataSet; // 关联的数据集
    FItems: TQFieldList; // 子字段列表
    FDataType: TFieldType; // 对应的VCL数据类型
    FPrecision: Word; // 精度
    FScale: Word; // 小数点后位置
    FSize: Integer; // 大小，如varchar(50),FSize就是50
    FNo: Word; // 字段顺序号
    FDBNo: Word; // 数据库中原始字段序列号
    FParent: TQField; // 父字段
    FIndex: Integer; // 在父字段中的顺序索引
    FOnCompare: TQValueCompare;
    FDisplayLabel: QStringW;
    FDisplayFormat: QStringW;
    FPrivateData: TBytes;
    FOwner: TComponent; // 指定字段值比较大小的方法，一般根据字段的类型会内部关联一个比较算法，以提高效率
    function CompareString(const V1, V2: TQValue; AIgnoreCase: Boolean)
      : Integer;
    function CompareShortInt(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareSmallInt(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareByte(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareInteger(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareWord(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareDWord(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareBoolean(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareFloat(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareSingle(const V1, V2: TQValue; AIgnoreCase: Boolean)
      : Integer;
    function CompareCurrency(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareBCD(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareInt64(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareWString(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareGuid(const V1, V2: TQValue; AIgnoreCase: Boolean): Integer;
    function CompareBinary(const V1, V2: TQValue; AIgnoreCase: Boolean)
      : Integer;
    function CompareUnsupport(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function CompareDateTime(const V1, V2: TQValue;
      AIgnoreCase: Boolean): Integer;
    function GetFieldItems(AIndex: Integer): TQField;
    procedure SetDataType(const Value: TFieldType);
    procedure SetField(const Value: TField);
    procedure SetFieldDef(const Value: TFieldDef);
    procedure SetParentField(const Value: TQField);
  protected
    function GetCount: Integer;
    function GetItems(const AIndex: Integer): IQField;
    function AddChild: IQField; overload;
    procedure DeleteChild(const AIndex: Integer);
    function GetSchema: QStringW;
    procedure SetSchema(const S: QStringW);
    function GetTable: QStringW;
    procedure SetTable(const S: QStringW);
    function GetDatabase: QStringW;
    procedure SetDatabase(const S: QStringW);
    function GetName: QStringW;
    procedure SetName(const S: QStringW);
    function GetBaseName: QStringW;
    procedure SetBaseName(const S: QStringW);
    function GetDBType: Integer;
    procedure SetDBType(const V: Integer);
    function GetOwner: IQDataProducer;
    function GetDataType: TFieldType;
    function GetPrecision: Word;
    procedure SetPrecision(const V: Word);
    function GetScale: Word;
    procedure SetScale(const V: Word);
    function GetSize: Integer;
    procedure SetSize(const V: Integer);
    function GetDBNo: Word;
    procedure SetDBNo(const V: Word);
    function GetFieldNo: Word;
    procedure SetFieldNo(const V: Word);
    function GetParent: IQField;
    function GetIndex: Integer;
    function GetInWhere: Boolean;
    procedure SetInWhere(const V: Boolean);
    function GetIsArray: Boolean;
    procedure SetIsArray(const V: Boolean);
    function GetIsAutoInc: Boolean;
    procedure SetIsAutoInc(const V: Boolean);
    function GetIsCalc: Boolean;
    procedure SetIsCalc(const V: Boolean);
    function GetIsFixed: Boolean;
    procedure SetIsFixed(const V: Boolean);
    function GetIsIndex: Boolean;
    procedure SetIsIndex(const V: Boolean);
    function GetIsPrimary: Boolean;
    procedure SetIsPrimary(const V: Boolean);
    function GetIsUnique: Boolean;
    procedure SetIsUnique(const V: Boolean);
    function GetNullable: Boolean;
    procedure SetNullable(const V: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const V: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(const V: Boolean);
    function GetPrivateData: Pointer;
    procedure SetPrivateData(const p: Pointer; ASize: Integer);
  public
    constructor Create(Owner: TComponent); overload;
    destructor Destroy; override;
    function Add: TQField; overload;
    procedure Delete(const AIndex: Integer);
    procedure Clear;
    function IndexOfName(const S: QStringW): Integer;
    class function DBTypeToDataType(ADBType: Word): TFieldType;
    class function DataTypeToDBType(AFieldType: TFieldType): Word;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQField read GetFieldItems; default;
    // 下面的属性对于非本单元来说是只读的
    property Schema: QStringW read FSchema write FSchema;
    property Database: QStringW read FDatabase write FDatabase;
    property Table: QStringW read FTable write FTable;
    property BaseName: QStringW read FBaseName write FBaseName;
    property Name: QStringW read FName write FName;
    property DisplayLabel: QStringW read FDisplayLabel write FDisplayLabel;
    property DisplayFormat: QStringW read FDisplayFormat write FDisplayFormat;
    property DBType: Integer read FDBType write SetDBType;
    property FieldDef: TFieldDef read FFieldDef write SetFieldDef;
    property Field: TField read FField write SetField;
    property Owner: TComponent read FOwner;
    property DataType: TFieldType read FDataType write SetDataType;
    property Precision: Word read FPrecision write SetPrecision;
    property Scale: Word read FScale write SetScale;
    property Size: Integer read FSize write SetSize;
    property No: Word read FNo write FNo;
    property DBNo: Word read FDBNo write FDBNo;
    property Parent: TQField read FParent write SetParentField;
    property Index: Integer read FIndex;
    property IsPrimary: Boolean read GetIsPrimary write SetIsPrimary;
    property IsIndex: Boolean read GetIsIndex write SetIsIndex;
    property IsUnique: Boolean read GetIsUnique write SetIsUnique;
    property Nullable: Boolean read GetNullable write SetNullable;
    property IsFixed: Boolean read GetIsFixed write SetIsFixed;
    property IsAutoInc: Boolean read GetIsAutoInc write SetIsAutoInc;
    property Visible: Boolean read GetVisible write SetVisible;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property IsCalc: Boolean read GetIsCalc write SetIsCalc;
    property HasChildren: Boolean read GetIsArray write SetIsArray;
    property InWhere: Boolean read GetInWhere write SetInWhere;
  end;

  // 元数据信息记录
  TQSchema = class
  private
    FName: QStringW; // 名称
    FItems: TQSchemaList; // 子项目列表
    FParent: TQSchema; // 父对象
    FFlags: Cardinal; // 元数据信息标志
    FSize: Word; // 元数据大小（字段）
    FDBType: Integer; // 数据库原始类型
    FPrecision: Word; // 精度
    FScale: Word; // 小数点位数
    function GetIsDatabase: Boolean;
    function GetIsField: Boolean;
    function GetIsFixed: Boolean;
    function GetIsIndex: Boolean;
    function GetIsPrimary: Boolean;
    function GetIsSchema: Boolean;
    function GetIsTable: Boolean;
    function GetIsType: Boolean;
    function GetNullable: Boolean;
    procedure SetIsDatabase(const Value: Boolean);
    procedure SetIsField(const Value: Boolean);
    procedure SetIsFixed(const Value: Boolean);
    procedure SetIsIndex(const Value: Boolean);
    procedure SetIsPrimary(const Value: Boolean);
    procedure SetIsSchema(const Value: Boolean);
    procedure SetIsTable(const Value: Boolean);
    procedure SetIsType(const Value: Boolean);
    procedure SetNullable(const Value: Boolean);
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQSchema;
  public
    constructor Create; overload;
    constructor Create(AName: QStringW; ATypeFlags: Cardinal = 0); overload;
    destructor Destroy; override;
    function Find(APath: QStringW): TQSchema; // 查找
    function Add(AName: QStringW; ATypeFlags: Cardinal = 0): TQSchema; overload;
    // 添加
    procedure Add(AItem: TQSchema); overload; // 添加
    procedure Clear; // 清除
    property Name: QStringW read FName write FName; // 元数据名称
    property IsDatabase: Boolean read GetIsDatabase write SetIsDatabase;
    // 是否是数据库名
    property IsSchema: Boolean read GetIsSchema write SetIsSchema; // 是否是架构名
    property IsTable: Boolean read GetIsTable write SetIsTable; // 是否是表名
    property IsField: Boolean read GetIsField write SetIsField; // 是否是字段名
    property IsType: Boolean read GetIsType write SetIsType; // 是否是类型名
    property IsIndex: Boolean read GetIsIndex write SetIsIndex; // 是否是索引
    property IsPrimary: Boolean read GetIsPrimary write SetIsPrimary; // 是否是主键
    property Nullable: Boolean read GetNullable write SetNullable; // 是否允许为空
    property IsFixed: Boolean read GetIsFixed write SetIsFixed; // 是否是固定长度
    property Size: Word read FSize write FSize; // 大小
    property DBType: Integer read FDBType write FDBType; // 数据库原始类型
    property Precision: Word read FPrecision write FPrecision; // 精度
    property Scale: Word read FScale write FScale; // 小数点位数
    property Items[AIndex: Integer]: TQSchema read GetItems; // 子项目
    property Count: Integer read GetCount; // 子项目数
    property Parent: TQSchema read FParent; // 父项目
  end;

  TQLocalCheck = class(TCollectionItem)
  protected
  end;

  TQLocalChecks = class(TCollection)

  end;

  IQDataProducer = interface
    // GetRootField返回根字段，以便添加各个子段的原始定义信息
    function GetRootField: IQField;
    // 用于添加一条记录
    function AddRecord(ARecord: IQRecord): Boolean;
    // 分配一条记录
    function AllocRecord: IQRecord;
    // 获取当前记录
    function GetRecords(AIndex: Integer): IQRecord;
    // 获取指定编号的记录
    function GetRecordCount: Integer;
    // 获取变更的记录
    function GetChanges(AIndex: Integer): IQRecord;
    // 获取变更的记录数量
    function GetChangedCount: Integer;
  end;

  TQDataSet = class(TDataSet, IQDataProducer)
  private
    procedure SetProvider(const Value: TQProvider);
  protected
    FProvider: TQProvider;
    FHandle: THandle;
    FRootField: TQField;
    function GetRootField: IQField;
    function AddRecord(ARecord: IQRecord): Boolean;
    function AllocRecord: IQRecord;
    function GetRecords(AIndex: Integer): IQRecord;
    function GetRecordCount: Integer;
    function GetChanges(AIndex: Integer): IQRecord;
    function GetChangedCount: Integer;
    procedure FetchAllRecords;
  published
    property Provider: TQProvider read FProvider write SetProvider;
  end;

  TQConvertStep = (csBeforeImport, csLoadFields, csLoadData, csAfterImport,
    csBeforeExport, csSaveFields, csSaveData, csAfterExport);
  TMemoryDataConveterProgress = procedure(ASender: TQConverter;
    AStep: TQConvertStep; AProgress, ATotal: Integer) of object;
  { 导出范围选项
    merMeta ：元数据（也就是字段定义）
    merUnmodified : 未修改的数据
    merInserted : 新插入的数据
    merModified : 已变更的数据
    merDeleted : 已删除的数据
    merByFiltered : 仅在过滤范围内的数据
    merByPage : 按当前分页中的内容被导出
    merByCurrentOnly : 仅当前记录被导出
    如果merByFilter,merByPage,merByCurrentOnly三个选项同时存在，则优先级依次升高
    如[merByFilter,merByPage]等价于merByPage,[merByFilter,merByCurrentOnly]等价于
    merByCurrentOnly
  }
  TQExportRange = (merMeta, merUnmodified, merInserted, merModified, merDeleted,
    merByFiltered, merByPage, merByCurrentOnly);
  TQExportRanges = set of TQExportRange;

  TQConverter = class(TComponent, IQDataProducer)
  protected
    FExportRanges: TQExportRanges;
    FOnProgress: TMemoryDataConveterProgress;
    FStream: TStream;
    procedure SaveToStream(AProvider: TQProvider; AResult: THandle;
      AStream: TStream); overload;
    // GetRootField返回根字段，以便添加各个子段的原始定义信息
    function GetRootField: IQField;
    // 用于添加一条记录
    function AddRecord(ARecord: IQRecord): Boolean;
    // 分配一条记录
    function AllocRecord: IQRecord;
    // 获取当前记录
    function GetRecords(AIndex: Integer): IQRecord;
    // 获取指定编号的记录
    function GetRecordCount: Integer;
    // 获取变更的记录
    function GetChanges(AIndex: Integer): IQRecord;
    // 获取变更的记录数量
    function GetChangedCount: Integer;
    procedure BeforeImport; virtual;
    procedure AfterImport; virtual;
    procedure BeforeExport; virtual;
    procedure AfterExport; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(ADataSet: TQDataSet; AStream: TStream);
    // 从流中加载
    procedure SaveToStream(ADataSet: TQDataSet; AStream: TStream); overload;
    // 保存数据集到流中
    procedure LoadFromFile(ADataSet: TQDataSet; AFileName: WideString);
    // 从文件中加载
    procedure SaveToFile(ADataSet: TQDataSet; AFileName: WideString);
    // 保存到文件中
    property ExportRanges: TQExportRanges read FExportRanges
      write FExportRanges; // 导出范围选择
    property OnProgress: TMemoryDataConveterProgress read FOnProgress
      write FOnProgress;
  end;

  /// <summary>事务隔离级别，具体支持程度由数据库本身决定，如果不支持，会转换为比较接近的级别</summary>
  /// dilUnspecified - 未指定，一般会转换为dilReadCommited
  /// dilReadCommited - 读取已经提交的数据
  /// dilReadUncommited - 允许读取未提交的数据（脏读）
  /// dilRepeatableRead - 循环读
  /// dilSerializable - 串行
  /// dilSnapshot - 快照

  TQDBIsolationLevel = (dilUnspecified, dilReadCommitted, dilReadUncommitted,
    dilRepeatableRead, dilSerializable, dilSnapshot);

  /// <summary>TQCommand用于SQL脚本相关信息记录</summary>
  TQCommand = record
    Name: QStringW;
    DataObject: IQDataProducer;
    SQL: QStringW;
    Params: TParams;
    Prepared: Boolean;
  end;

  TQExecuteStatics = record
    QueuedTime: Int64; // 请求投寄时间
    StartTime: Int64; // 请求开始时间
    PreparedTime: Int64; // 准备就绪时间
    ExecuteStartTime: Int64; // 脚本开始执行时间
    ExecuteDoneTime: Int64; // 执行完成时间
    StopTime: Int64; // 执行完成时间
    LoadedTime: Int64; // 加载完成
    AffectRows: Integer; // 影响的行数
  end;

  TQExecuteResult = record
    Statics: TQExecuteStatics; // 运行统计信息
    ErrorCode: Cardinal; // 错误代码
    ErrorMsg: QStringW; // 错误提示
  end;

  /// <summary>命令被发往数据库执行脚本前触发的事件</summary>
  /// <params>
  /// <param name="ASender">当前执行的Provider对象</param>
  /// <param name="ACommand">当前要执行的脚本对象</param>
  /// </params>
  TQBeforeExecuteEvent = procedure(ASender: TQProvider;
    const ACommand: TQCommand) of object;
  /// <summary>在命令执行完成后触发的事件</summary>
  /// <params>
  /// <param name="ASender">提供者对象</param>
  /// <param name="ACommand">执行的命令对象</param>
  /// <param name="AResult">执行结果</param>
  /// </params>
  TQAfterExecuteEvent = procedure(ASender: TQProvider;
    const ACommand: TQCommand; const AResult: TQExecuteResult) of object;

  // 内部记录执行参数的相关记录
  TQSQLRequest = record
    Command: TQCommand; // 命令
    WaitResult: Boolean; // 如果不为空，则等待结果完成，如果为空，则不等待
    Result: TQExecuteResult; // 执行结果
    AfterOpen: TQAfterExecuteEvent; // 执行完成回调事件
  end;

  /// <summary>通知信息级别</summary>
  TQNoticeLevel = (nlLog, nlInfo, nlDebug, nlNotice, nlWarning, nlError,
    nlPanic, nlFatal);

  TQServerNotificationEvent = procedure(ASender: TQProvider;
    ALevel: TQNoticeLevel; const AMsg: QStringW) of object;

  TQProvider = class(TComponent)
  protected
    FProviderName: QStringW; // 唯一名称标志
    FDatabase: QStringW; // 数据库表
    FSchema: QStringW; // 连接到的模式名
    FErrorCode: Cardinal; // 末次错误代码
    FErrorMsg: QStringW; // 末次错误消息
    // 连接相关事件
    FBeforeConnect: TNotifyEvent;
    FAfterConnected: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnParamChanged: TNotifyEvent;
    // 执行相关事件
    FBeforeExecute: TQBeforeExecuteEvent; // 执行脚本前触发的事件
    FAfterExecute: TQAfterExecuteEvent; // 执行脚本完成后触发的事件
    FOnServerNotification: TQServerNotificationEvent;
    FTransactionLevel: Integer; // 事务嵌套级别
    FParams: TStringList; // 命令参数
    FHandle: THandle; // 由底层驱动返回的连接句柄
    FCommandTimeout: Cardinal; // 命令超时
    FConnectionString: QStringW; // 连接字符串
    FCached: TQDataSetList; // 缓存的数据集对象
    FDataSets: TQDataSetList; // 关联的数据集对象
    FPeekInterval: Integer;
    FFlags: Integer;
    procedure DoParamChanged(); virtual;
    procedure SetConnectionString(const Value: QStringW);
    { 释放一个由Execute返回的结果句柄
      Parameters
      AHandle :  要释放的句柄，由Execute函数返回 }
    procedure DestroyHandle(AHandle: THandle); virtual; abstract;
    procedure SetError(ACode: Cardinal; const AMsg: QStringW);
    procedure SetParams(const Value: TStrings);
    procedure SetConnected(const Value: Boolean);
    { 获取指定的连接字符串列表，注意内部使用UTF8编码，如果包含中文等字符应先转为UTF8编码 }
    function GetParams: TStrings;
    function GetConnected: Boolean;
    { 获取指定结果集总记录数
      Parameters
      AHandle :  由Execute返回的结果句柄

      Returns
      返回实际的记录数 }
    function GetRecordCount(AHandle: THandle): Integer; virtual; abstract;
    { 返回结果中包含的字段数量
      Parameters
      AHandle :  由Execute返回的结果句柄 }
    function GetFieldCount(AHandle: THandle): Integer; virtual; abstract;
    { 获取指定的结果句柄中，受影响的记录行数
      Parameters
      AHandle :  由Execute返回的结果句柄

      Returns
      返回受影响的行数 }
    function GetAffectedCount(AHandle: THandle): Integer; virtual; abstract;
    { 获取指定的字段定义
      Parameters
      AHandle :  由Execute返回的结果句柄
      AIndex :   字段索引
      ADef :     返回的实际定义

      Returns
      成功，返回true，失败，返回false }
    function GetFieldDef(AHandle: THandle; AIndex: Integer; var ADef: TQField)
      : Boolean; virtual; abstract;
    { 获取指定字段的内容
      Parameters
      AHandle :    由Execute返回的结果句柄
      ARowIndex :  记录行号
      AField :     字段定义
      AVal :       具体内容

      Returns
      如果成功获取到值（即使是空值，也是成功获取到），返回true，否则，返回false }
    function GetFieldData(AHandle: THandle; ARowIndex: Integer; AField: TQField;
      AVal: TQValue): Boolean; virtual; abstract;
    /// <summary>执行指定的脚本</summary>
    /// <params>
    /// <param name="ARequest">命令执行参数</param>
    /// </params>
    /// <returns>成功，返回原始结果句柄，失败，返回-1。</returns>
    function InternalExecute(var ARequest: TQSQLRequest): THandle; virtual;
      abstract;
    { 执行实际的关闭连接动作 }
    procedure InternalClose; virtual; abstract;
    { 执行实际的建立连接动作 }
    procedure InternalOpen; virtual; abstract;
    { 内部执行实际的更新操作
      Parameters
      ARootField :  根字段，它可能隶属于一个数据集，也可能是临时创建的一个对象
      ARecords :    要更新的记录列表 }
    procedure InternalApplyUpdates(ARootField: TQField;
      ARecords: TQRecords); virtual;
    { 组件加载完成后，检查属性，以确定是否连接（如Connected在设计时设置为True) }
    procedure Loaded; override;
    /// <summary>执行指定的脚本</summary>
    /// <params>
    /// <param name="ARequest">命令执行参数</param>
    /// </params>
    /// <returns>成功，返回原始结果句柄，失败，返回-1。</returns>
    function Execute(var ARequest: TQSQLRequest): THandle; virtual;
    procedure KeepAliveNeeded; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetPeekInterval(const Value: Integer);
    procedure DoLivePeek(AJob: PQJob);
    function GetConnectionString: QStringW;
    function GetFlags(const Index: Integer): Boolean;
    procedure SetFlags(AFlag: Integer; AValue: Boolean);
    procedure InternalSetParams(ADest: TParams; const ASource: array of const);
    procedure InitializeRequest(var ARequest: TQSQLRequest;
      const ASQL: QStringW; ACreateParams: Boolean);
    procedure InternalApplyUpdate(ADataSource: IQDataProducer); virtual;
    function PrepareChangeRequest(var ARequest: TQSQLRequest;
      AUpdatStatus: TUpdateStatus): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { 内部使用，析构前进行一些清理工作 }
    procedure BeforeDestruction; override;
    /// <summary>打开连接</summary>
    /// <returns>如果成功，返回true，如果失败，返回false</returns>
    function Open: Boolean;
    // OpenStream函数

    function OpenStream(ACmdText: QStringW; AStreamFormat: TQConverterClass)
      : TMemoryStream; overload;
    function OpenStream(AStream: TStream; ACmdText: QStringW;
      AStreamFormat: TQConverterClass): Boolean; overload; virtual;
    function OpenStream(AStream: TStream; ASQL: TQCommand;
      AStreamFormat: TQConverterClass): Boolean; overload; virtual;
    function OpenStream(ASQL: TQCommand; AStreamFormat: TQConverterClass)
      : TMemoryStream; overload;
    function OpenStream(AStream: TStream; ACmdText: QStringW;
      AStreamFormat: TQConverterClass; AParams: array of const)
      : Boolean; overload;
    function OpenStream(ACmdText: QStringW; AStreamFormat: TQConverterClass;
      AParams: array of const): TMemoryStream; overload;
    function OpenStream(ACmdText: QStringW; AStreamFormat: TQConverter)
      : TMemoryStream; overload;
    function OpenStream(AStream: TStream; ACmdText: QStringW;
      AStreamFormat: TQConverter): Boolean; overload; virtual;
    function OpenStream(AStream: TStream; ASQL: TQCommand;
      AStreamFormat: TQConverter): Boolean; overload; virtual;
    function OpenStream(ASQL: TQCommand; AStreamFormat: TQConverter)
      : TMemoryStream; overload;
    function OpenStream(AStream: TStream; ACmdText: QStringW;
      AStreamFormat: TQConverter; AParams: array of const): Boolean; overload;
    function OpenStream(ACmdText: QStringW; AStreamFormat: TQConverter;
      AParams: array of const): TMemoryStream; overload;

    // OpenDataSet函数

    function OpenDataSet(ACmdText: QStringW): TQDataSet; overload;
    function OpenDataSet(ADataSet: TQDataSet; ACmdText: QStringW;
      AfterOpen: TQAfterExecuteEvent = nil): Boolean; overload; virtual;
    function OpenDataSet(ADataSet: TQDataSet; ASQL: TQCommand;
      AfterOpen: TQAfterExecuteEvent = nil): Boolean; overload; virtual;
    function OpenDataSet(ADataSet: TQDataSet; ACmdText: QStringW;
      AParams: array of const; AfterOpen: TQAfterExecuteEvent = nil)
      : Boolean; overload;
    function OpenDataSet(ACmdText: QStringW; AParams: array of const)
      : TQDataSet; overload;

    // ExecuteCmd函数

    function ExecuteCmd(ACmdText: QStringW): Integer; overload; virtual;
    function ExecuteCmd(AParams: TQCommand): Integer; overload; virtual;
    function ExecuteCmd(ACmdText: QStringW; const AParams: array of const)
      : Integer; overload;

    // Prepare
    /// <summary>准备一个SQL脚本，以便重复执行</summary>
    /// <params>
    /// <param name="ACmdText">要执行的SQL脚本</param>
    /// </params>
    /// <returns>成功，返回准备完成的SQL语句对象，可以用于OpenStream/OpenDataSet/ExecuteCmd语句</returns>
    /// <remarks>返回的TQCommand对象需要手动释放
    function Prepare(ACmdText: QStringW; AName: QStringW = '')
      : TQCommand; virtual;
    /// <summary> 开启事务或保存点</summary>
    /// <params>
    /// <param name="ALevel">新事务的事务隔离级别，默认为diUnspecified，由数据库决定</param>
    /// <param name="ASavePointName">数据库事务保存点名称</param>
    /// </params>
    /// <returns>成功开启事务，返回true，否则，返回false</returns>
    function BeginTrans(ALevel: TQDBIsolationLevel = dilUnspecified;
      ASavePointName: QStringW = ''): Boolean; virtual;

    { 判断是否存在指定的物理表（不包含视图）
      Parameters
      ATableName :  要判断的表名

      Returns
      存在，返回true，不存在，返回false }
    function TableExists(ATableName: QStringW): Boolean; virtual;
    { 判断指定的视图是否存在
      Parameters
      AName :  要判断的视图名称

      Returns
      存在，返回true，不存在，返回false }
    function ViewExists(AName: QStringW): Boolean; virtual;
    { 判断指定的函数是否存在
      Parameters
      AName :  要判断的函数名称 }
    function FunctionExists(AName: QStringW): Boolean; virtual;
    { 判断是否存在指定的存贮过程
      Parameters
      AName :  要判断的存贮过程名

      Returns
      存在，返回true，否则，返回false }
    function ProcedureExists(AName: QStringW): Boolean; virtual;
    { 判断指定的触发器是否存在
      Parameters
      AName :  要判断的触发器名称

      Returns
      存在，返回true，不存在，返回false }
    function TriggerExists(AName: QStringW): Boolean; virtual;
    { 判断指定表的指定字段是否存在,如果存在，返回true
      Parameters
      ATableName :  表名
      AColName :    列名 }
    function ColumnExists(ATableName, AColName: QStringW): Boolean; virtual;
    { 判断指定的脚本是否返回至少一条记录
      Parameters
      ACmdText :  要执行的脚本

      Returns
      如果返回一个结果集，并且至少返回一条记录，则返回True，否则返回false


      Remarks
      传递的命令脚本是实际被执行的，因此，如果包含了修改数据的命令，请做好相应的事务处理。 }
    function RecordExists(ACmdText: QStringW): Boolean;
    procedure CommitTrans; virtual; // 提交事务
    procedure RollbackTrans(ASavePointName: QStringW = ''); virtual;
    { 回滚事务或保存点
      Parameters
      ASavePointName :  保存点名称，为空表示还原事务，否则是还原保存点 }
    procedure Close; // 关闭连接
    procedure ApplyUpdates(ADataSet: TQDataSet); overload; virtual;
    { 将指定数据集的变更内容应用到数据库中
      Parameters
      ADataSet :  要更新的数据集对象 }
    { 将指定流中的变更内容应用到数据库中
      Parameters
      AStream :  源数据流
      AFormat :  数据流内容的格式转换器类型 }
    procedure ApplyUpdates(AStream: TStream; AFormat: TQConverterClass);
      overload; virtual;
    { 将指定文件中的变更信息应用
      Parameters
      AFileName :  文件名
      AFormat :    文件格式转换器类型 }
    procedure ApplyUpdates(AFileName: QStringW; AFormat: TQConverterClass);
      overload; virtual;

    { 从缓存中分配一个数据集对象，如果不存在，就创建一个新的数据集对象返回。 }
    function AcquireDataSet: TQDataSet;
    { 将一个由OpenDataSet或AcquireDataSet返回的数据集对象交还回缓冲池 }
    procedure ReleaseDataSet(ADataSet: TQDataSet);
    property ProviderName: QStringW read FProviderName; // 名称标志
    property LastError: Cardinal read FErrorCode; // 末次错误代码
    property LastErrorMsg: QStringW read FErrorMsg; // 末次错误消息内容
    property TransactionLevel: Integer read FTransactionLevel;
    // 事务隔离级别;//事务隔离级别
    property Handle: THandle read FHandle;
  published
    property BeforeExecute: TQBeforeExecuteEvent read FBeforeExecute
      write FBeforeExecute; // 执行脚本前触发事件
    property AfterExecute: TQAfterExecuteEvent read FAfterExecute
      write FAfterExecute; // 执行脚本后触发事件
    property BeforeConnect: TNotifyEvent read FBeforeConnect
      write FBeforeConnect; // 连接建立前触发
    property AfterConnected: TNotifyEvent read FAfterConnected
      write FAfterConnected; // 连接建立后触发
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect
      write FBeforeDisconnect; // 连接断开前触发
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect
      write FAfterDisconnect; // 连接断开后触发
    property OnParamChanged: TNotifyEvent read FOnParamChanged
      write FOnParamChanged;
    property OnServerNotification: TQServerNotificationEvent
      read FOnServerNotification write FOnServerNotification;
    property ConnectParams: TStrings read GetParams write SetParams;
    // 连接参数，注意使用的应为UTF8编码
    property Connected: Boolean read GetConnected write SetConnected; // 是否已连接
    property ConnectionString: QStringW read GetConnectionString
      write SetConnectionString; // 连接字符串
    property CommandTimeout: Cardinal read FCommandTimeout write FCommandTimeout
      default 30; { 命令执行超时时间，对于部分提供者对象，可能无意义 }
    property Connecting: Boolean index PF_CONNECTING read GetFlags; // 是否正在连接数据库
    property Closing: Boolean index PF_CLOSING read GetFlags;
    property Executing: Boolean index PF_EXECUTING read GetFlags;
    property Peeking: Boolean index PF_PEEKING read GetFlags;
    property KeepAlive: Boolean index PF_KEEPALIVE read GetFlags;
    property PeekInterval: Integer read FPeekInterval write SetPeekInterval;
  end;

implementation

uses math;

resourcestring
  SBadTypeConvert = '无效的类型转换:%s->%s';
  SCantCompareField = '指定的字段类型 [%s] 不能进行比较操作。';
  SOutOfRange = '索引 %d 越界。';
  SUnsupportDataType = '不支持的字段类型 %s';
  SNotArrayType = '%s 不是ftArray,ftObject,ftADT之一。';
  SNotConnected = '未连接到数据库，请先连接到数据库。';
  SEmptySQL = '未指定要执行的SQL脚本内容.';
  SUpdateNotSupport = '[%s] 对应的驱动程序不支持更新操作。';
  SCantConnectToDB = '无法建立与数据库的连接，错误代码:%d,错误信息:'#13#10'%s';
  SUnsupportParamValue = '不支持的参数值类型';

const
  SQLTypeMap: array [TFieldType] of Integer = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord,// 0..4
    SQL_UNKNOWN, SQL_VARCHAR, SQL_SMALLINT, SQL_INTEGER, SQL_WORD,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,// 5..11
    SQL_BOOLEAN, SQL_FLOAT, SQL_MONEY, SQL_BCD, SQL_DATE, SQL_TIME,
    SQL_DATETIME,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    SQL_BYTES, SQL_VARBYTES, SQL_SERIAL, SQL_LARGEOBJECT, SQL_TEXT,
    SQL_PICTURE, SQL_TEXT,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftQStringW, // 19..24
    SQL_OLE, SQL_OLE, SQL_LARGEOBJECT, SQL_CURSOR, SQL_CHAR, SQL_WIDEVARCHAR,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    SQL_INT64, SQL_ADT, SQL_ARRAY, SQL_REFERENCE, SQL_DATASET, SQL_LARGEOBJECT,
    SQL_WIDETEXT,
    // ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    SQL_VARIANT, SQL_INTERFACE, SQL_IDISPATCH, SQL_GUID, SQL_TIMESTAMP, SQL_BCD,
    // ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
    SQL_WIDECHAR, SQL_WIDETEXT, SQL_TIMESTAMP, SQL_INTERVAL,
    // ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
    SQL_DWORD, SQL_TINYINT, SQL_BYTE, SQL_EXTENDED, SQL_CONNECTION, SQL_PARAMS,
    SQL_STREAM,
    // ftTimeStampOffset, ftObject, ftSingle
    SQL_TIMEOFFSET, SQL_OBJECT, SQL_SINGLE);

{ TQField }

function TQField.Add: TQField;
begin
if not Assigned(FItems) then
  FItems := TQFieldList.Create;
Result := TQField.Create;
Result.FParent := Self;
FItems.Add(Result);
end;

function TQField.AddChild: IQField;
begin

end;

procedure TQField.Clear;

var
  I: Integer;
begin
if Assigned(FItems) then
  begin
  for I := 0 to Count do
    FreeObject(Items[I]);
  FItems.Clear;
  end;
end;

function TQField.CompareBCD(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := BcdCompare(V1.AsBcd, V2.AsBcd);
end;

function TQField.CompareBinary(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  b1, b2: TBytes;
  L1, L2: Integer;
begin
b1 := V1.AsBytes;
b2 := V2.AsBytes;
L1 := Length(b1);
L2 := Length(b2);
if L1 = L2 then
  Result := BinaryCmp(@b1[0], @b2[0], L1)
else if L1 < L2 then
  begin
  Result := BinaryCmp(@b1[0], @b2[0], L1);
  if Result = 0 then
    Result := -1;
  end
else
  begin
  Result := BinaryCmp(@b1[0], @b2[0], L2);
  if Result = 0 then
    Result := 1;
  end;
end;

function TQField.CompareBoolean(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := Integer(V1.AsBoolean) - Integer(V2.AsBoolean);
end;

function TQField.CompareByte(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsByte - V2.AsByte;
end;

function TQField.CompareCurrency(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  C1, C2: Currency;
begin
C1 := V1.AsCurrency;
C2 := V2.AsCurrency;
Result := PInt64(@C1)^ - PInt64(@C2)^;
end;

function TQField.CompareDateTime(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := CompareFloat(V2, V2, AIgnoreCase);
end;

function TQField.CompareDWord(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsDWord - V2.AsDWord;
end;

function TQField.CompareFloat(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  D1, D2: Double;
begin
D1 := V1.AsFloat;
D2 := V2.AsFloat;
if D1 - D2 > 0 then
  Result := 1
else if D2 - D1 > 0 then
  Result := -1
else
  Result := 0;
end;

function TQField.CompareGuid(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  G1, G2: TGuid;
begin
G1 := V1.AsGuid;
G2 := V2.AsGuid;
Result := BinaryCmp(@G1, @G2, Sizeof(TGuid));
end;

function TQField.CompareInt64(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsInt64 - V2.AsInt64;
end;

function TQField.CompareInteger(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsInteger - V2.AsInteger;
end;

function TQField.CompareShortInt(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsShortint - V2.AsShortint;
end;

function TQField.CompareSingle(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  ADelta: Single;
begin
ADelta := V1.AsSingle - V2.AsSingle;
if ADelta > 0 then
  Result := 1
else if ADelta < 0 then
  Result := -1
else
  Result := 0;
end;

function TQField.CompareSmallInt(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsSmallint - V2.AsSmallint;
end;

function TQField.CompareString(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  S1, S2: QStringW;
begin
S1 := V1.AsString;
S2 := V2.AsString;
Result := StrCmpW(PQCharW(S1), PQCharW(S2), AIgnoreCase);
end;

function TQField.CompareUnsupport(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
DatabaseError(Format(SCantCompareField, [FieldTypeNames[FDataType]]));
end;

function TQField.CompareWord(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;
begin
Result := V1.AsWord - V2.AsWord;
end;

function TQField.CompareWString(const V1, V2: TQValue;
  AIgnoreCase: Boolean): Integer;

var
  S1, S2: QStringW;
begin
S1 := V1.AsString;
S2 := V2.AsString;
Result := StrCmpW(PQCharW(S1), PQCharW(S2), AIgnoreCase);
end;

constructor TQField.Create(Owner: TComponent);
begin
FDBType := SQL_UNKNOWN;
FFlags := 0;
FItems := nil;
FDataType := ftUnknown;
//FOnCompare := CompareUnsupport;
FOwner := Owner;
end;

class function TQField.DataTypeToDBType(AFieldType: TFieldType): Word;
begin

end;

class function TQField.DBTypeToDataType(ADBType: Word): TFieldType;
begin

end;

procedure TQField.Delete(const AIndex: Integer);
begin

end;

procedure TQField.DeleteChild(const AIndex: Integer);
begin

end;

destructor TQField.Destroy;
begin
if Assigned(FItems) then
  begin
  Clear;
  FreeObject(FItems);
  end;
inherited;
end;

function TQField.GetBaseName: QStringW;
begin

end;

function TQField.GetCount: Integer;
begin
if Assigned(FItems) then
  Result := FItems.Count
else
  Result := 0;
end;

function TQField.GetDatabase: QStringW;
begin

end;

function TQField.GetDataType: TFieldType;
begin

end;

function TQField.GetDBNo: Word;
begin

end;

function TQField.GetDBType: Integer;
begin

end;

function TQField.GetFieldItems(AIndex: Integer): TQField;
begin

end;

function TQField.GetFieldNo: Word;
begin

end;

function TQField.GetIndex: Integer;
begin

end;

function TQField.GetInWhere: Boolean;
begin
Result := (FFlags and SCHEMA_INWHERE) <> 0;
end;

function TQField.GetIsArray: Boolean;
begin
Result := (FFlags and SCHEMA_ARRAY) <> 0;
end;

function TQField.GetIsAutoInc: Boolean;
begin
Result := (FFlags and SCHEMA_AUTOINC) <> 0;
end;

function TQField.GetIsCalc: Boolean;
begin
Result := (FFlags and SCHEMA_CALC) <> 0;
end;

function TQField.GetIsFixed: Boolean;
begin
Result := (FFlags and SCHEMA_ISFIXED) <> 0;
end;

function TQField.GetIsIndex: Boolean;
begin
Result := (FFlags and SCHEMA_ISINDEX) <> 0;
end;

function TQField.GetIsPrimary: Boolean;
begin
Result := (FFlags and SCHEMA_ISPRIMARY) <> 0;
end;

function TQField.GetIsUnique: Boolean;
begin
Result := (FFlags and SCHEMA_UNIQUE) <> 0;
end;

function TQField.GetItems(const AIndex: Integer): IQField;
begin
if Assigned(FItems) then
  Result := FItems[AIndex]
else
  DatabaseError(Format(SOutOfRange, [AIndex]));
end;

function TQField.GetName: QStringW;
begin

end;

function TQField.GetNullable: Boolean;
begin
Result := (FFlags and SCHEMA_NULLABLE) <> 0;
end;

function TQField.GetOwner: IQDataProducer;
begin

end;

function TQField.GetParent: IQField;
begin

end;

function TQField.GetPrecision: Word;
begin

end;

function TQField.GetPrivateData: Pointer;
begin
if Length(FPrivateData) > 0 then
  Result := @FPrivateData[0]
else
  Result := nil;
end;

function TQField.GetReadOnly: Boolean;
begin
Result := (FFlags and SCHEMA_READONLY) <> 0;
end;

function TQField.GetScale: Word;
begin

end;

function TQField.GetSchema: QStringW;
begin

end;

function TQField.GetSize: Integer;
begin

end;

function TQField.GetTable: QStringW;
begin

end;

function TQField.GetVisible: Boolean;
begin
Result := (FFlags and SCHEMA_VISIBLE) <> 0;
end;

function TQField.IndexOfName(const S: QStringW): Integer;
begin

end;

procedure TQField.SetBaseName(const S: QStringW);
begin

end;

procedure TQField.SetDatabase(const S: QStringW);
begin

end;

procedure TQField.SetDataType(const Value: TFieldType);
begin
if Value <> FDataType then
  begin
//  FDataType := Value;
//  case Value of
//    ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar,
//      ftWideMemo:
//      FOnCompare := CompareString;
//    ftSmallint:
//      FOnCompare := CompareSmallInt;
//    ftInteger:
//      FOnCompare := CompareInteger;
//    ftWord:
//      FOnCompare := CompareWord;
//    ftBoolean:
//      FOnCompare := CompareBoolean;
//    ftFloat:
//      FOnCompare := CompareFloat;
//    ftCurrency:
//      FOnCompare := CompareCurrency;
//    ftBCD:
//      FOnCompare := CompareBCD;
//    ftDate, ftTime, ftDateTime, ftTimeStamp, ftOraTimeStamp, ftOraInterval,
//      ftTimeStampOffset:
//      FOnCompare := CompareDateTime;
//    ftBytes, ftVarBytes, ftGraphic, ftTypedBinary:
//      FOnCompare := CompareBinary;
//    ftAutoInc:
//      FOnCompare := CompareInteger;
//    ftBlob, ftOraBlob, ftOraClob:
//      FOnCompare := CompareBinary;
//    ftLargeint:
//      FOnCompare := CompareInt64;
//    ftGuid:
//      FOnCompare := CompareGuid;
//    ftFMTBcd:
//      FOnCompare := CompareBCD;
//    ftLongword:
//      FOnCompare := CompareDWord;
//    ftShortint:
//      FOnCompare := CompareShortInt;
//    ftByte:
//      FOnCompare := CompareByte;
//    ftExtended:
//      FOnCompare := CompareFloat;
//    ftStream:
//      FOnCompare := CompareBinary;
//    ftSingle:
//      FOnCompare := CompareSingle
//  else
//    // ftUnknown,ftADT,ftArray,ftVariant,ftInterface,ftIDispatch:
//    FOnCompare := CompareUnsupport;
//  end;
  end;
end;

procedure TQField.SetDBNo(const V: Word);
begin

end;

procedure TQField.SetDBType(const V: Integer);
begin
if FDBType <> V then
  begin
  FDBType := V;
  FField := nil;
  FFieldDef := nil;
  end;
end;

procedure TQField.SetField(const Value: TField);
begin

FField := Value;
end;

procedure TQField.SetFieldDef(const Value: TFieldDef);
begin
FFieldDef := Value;
end;

procedure TQField.SetFieldNo(const V: Word);
begin

end;

procedure TQField.SetInWhere(const V: Boolean);
begin

end;

procedure TQField.SetIsArray(const V: Boolean);
begin

end;

procedure TQField.SetIsAutoInc(const V: Boolean);
begin

end;

procedure TQField.SetIsCalc(const V: Boolean);
begin

end;

procedure TQField.SetIsFixed(const V: Boolean);
begin

end;

procedure TQField.SetIsIndex(const V: Boolean);
begin

end;

procedure TQField.SetIsPrimary(const V: Boolean);
begin

end;

procedure TQField.SetIsUnique(const V: Boolean);
begin

end;

procedure TQField.SetName(const S: QStringW);
begin
if FName <> S then
  FName := S;
end;

procedure TQField.SetNullable(const V: Boolean);
begin

end;

procedure TQField.SetParentField(const Value: TQField);
begin
FParent := Value;
end;

procedure TQField.SetPrecision(const V: Word);
begin
FPrecision := V;
end;

procedure TQField.SetPrivateData(const p: Pointer; ASize: Integer);
begin
SetLength(FPrivateData, ASize);
Move(p^, FPrivateData[0], ASize);
end;

procedure TQField.SetReadOnly(const V: Boolean);
begin

end;

procedure TQField.SetScale(const V: Word);
begin
FScale := V;
end;

procedure TQField.SetSchema(const S: QStringW);
begin

end;

procedure TQField.SetSize(const V: Integer);
begin
FSize := V;
end;

procedure TQField.SetTable(const S: QStringW);
begin

end;

procedure TQField.SetVisible(const V: Boolean);
begin

end;

{ TQSchema }

function TQSchema.Add(AName: QStringW; ATypeFlags: Cardinal): TQSchema;
begin

end;

procedure TQSchema.Add(AItem: TQSchema);
begin

end;

procedure TQSchema.Clear;
begin

end;

constructor TQSchema.Create(AName: QStringW; ATypeFlags: Cardinal);
begin

end;

constructor TQSchema.Create;
begin

end;

destructor TQSchema.Destroy;
begin

inherited;
end;

function TQSchema.Find(APath: QStringW): TQSchema;
begin

end;

function TQSchema.GetCount: Integer;
begin

end;

function TQSchema.GetIsDatabase: Boolean;
begin

end;

function TQSchema.GetIsField: Boolean;
begin

end;

function TQSchema.GetIsFixed: Boolean;
begin

end;

function TQSchema.GetIsIndex: Boolean;
begin

end;

function TQSchema.GetIsPrimary: Boolean;
begin

end;

function TQSchema.GetIsSchema: Boolean;
begin

end;

function TQSchema.GetIsTable: Boolean;
begin

end;

function TQSchema.GetIsType: Boolean;
begin

end;

function TQSchema.GetItems(AIndex: Integer): TQSchema;
begin

end;

function TQSchema.GetNullable: Boolean;
begin

end;

procedure TQSchema.SetIsDatabase(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsField(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsFixed(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsIndex(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsPrimary(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsSchema(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsTable(const Value: Boolean);
begin

end;

procedure TQSchema.SetIsType(const Value: Boolean);
begin

end;

procedure TQSchema.SetNullable(const Value: Boolean);
begin

end;

{ TQColumnValue }

constructor TQColumnValue.Create(AField: TQField);
begin
FField := AField;
FOld := TQValue.Create(AField.DataType);
FNew := TQValue.Create(AField.DataType);
FCurrent := FOld;
end;

destructor TQColumnValue.Destroy;
begin
FreeObject(FOld);
FreeObject(FNew);
inherited;
end;

function TQColumnValue.GetChanged: Boolean;
begin
Result := (FCurrent = FNew);
end;

function TQColumnValue.GetCurrentValue: IQValue;
begin
Result := FCurrent;
end;

function TQColumnValue.GetNewValue: IQValue;
begin
Result := FNew;
end;

function TQColumnValue.GetOldValue: IQValue;
begin
Result := FOld;
end;

{ TQRecord }

constructor TQRecord.Create(AOwner: IQDataProducer);
var
  I, ACount: Integer;
  AProducer: IQDataProducer;
  ARootField: IQField;
begin
inherited Create;
FOwner := AOwner;
FOriginIndex := -1;
FItemIndex := -1;
FSortedIndex := -1;
FFilteredIndex := -1;
FRefCount := 0;
FBookmark := nil;
FStatus := usUnmodified;
AProducer := GetOwner;
if Assigned(AProducer) then
  begin
  ARootField := AProducer.GetRootField;
  ACount := ARootField.GetCount;
  SetLength(FValues, ACount);
  for I := 0 to ACount - 1 do
    FValues[I] := TQColumnValue.Create(ARootField.GetItems(I) as TQField);
  end
else
  SetLength(FValues, 0);
end;

destructor TQRecord.Destroy;

var
  C: Integer;
  I: Integer;
begin
C := High(FValues);
for I := 0 to C do
  FreeObject(FValues[I]);
inherited;
end;

function TQRecord.GetOwner: IQDataProducer;
begin
Result := FOwner;
end;

{ TQProvider }

function TQProvider.AcquireDataSet: TQDataSet;
begin
if FCached.Count > 0 then
  begin
  Result := FCached[0];
  FCached.Delete(0);
  end
else
  Result := TQDataSet.Create(nil);
end;

procedure TQProvider.ApplyUpdates(AFileName: QStringW;
  AFormat: TQConverterClass);
var
  AStream: TMemoryStream;
begin
AStream := TMemoryStream.Create;
try
  AStream.LoadFromFile(AFileName);
  ApplyUpdates(AStream, AFormat);
finally
  FreeObject(AStream);
end;
end;

procedure TQProvider.ApplyUpdates(ADataSet: TQDataSet);
begin
InternalApplyUpdate(ADataSet);
end;

procedure TQProvider.ApplyUpdates(AStream: TStream; AFormat: TQConverterClass);
var
  AConverter: TQConverter;
begin
AConverter := AFormat.Create(nil);
try
  AConverter.FStream := AStream;
  AConverter.BeforeImport;
  InternalApplyUpdate(AConverter);
  AConverter.AfterImport;
finally
  FreeObject(AConverter);
end;
end;

procedure TQProvider.BeforeDestruction;
var
  I: Integer;
  ADataSet: TQDataSet;
begin
if Connected then
  begin
  try
    Close;
  except
  end;
  end;
for I := 0 to FDataSets.Count - 1 do
  begin
  ADataSet := FDataSets[I];
  ADataSet.Provider := nil;
  end;
inherited;
end;

function TQProvider.BeginTrans(ALevel: TQDBIsolationLevel;
  ASavePointName: QStringW): Boolean;
begin
Result := True;
// 不支持事务，直接返回True
Inc(FTransactionLevel);
end;

procedure TQProvider.Close;
var
  I: Integer;
  ADataSet: TQDataSet;
begin
if FHandle <> 0 then
  begin
  SetFlags(PF_CLOSING, True);
  if Assigned(BeforeDisconnect) then
    BeforeDisconnect(Self);
  try
    for I := 0 to FDataSets.Count - 1 do
      begin
      ADataSet := TQDataSet(FDataSets[I]);
      if (ADataSet.FHandle <> 0) then
        begin
        ADataSet.FetchAllRecords;
        DestroyHandle(ADataSet.FHandle);
        ADataSet.FHandle := 0;
        end;
      end;
    InternalClose;
  finally
    FHandle := 0;
    FTransactionLevel := 0;
    SetFlags(PF_CONNECTED, False);
    SetFlags(PF_CLOSING, False);
    SetFlags(PF_CLOSED, True);
    if Assigned(AfterDisconnect) then
      AfterDisconnect(Self);
  end;
  // Workers.Clear(DoLivePeek);
  end;
end;

function TQProvider.ColumnExists(ATableName, AColName: QStringW): Boolean;

var
  ASchema, ACmdText: QStringW;
  pName: PQCharW;
begin
pName := PQCharW(ATableName);
ASchema := DecodeTokenW(pName, '.', '"', True);
ATableName := pName;
if Length(ATableName) > 0 then
  ACmdText := 'select * from information_schema.columns where table_schema=' +
    QuotedStrW(ASchema, '''') + ' and table_name=' + QuotedStrW(ATableName,
    '''') + ' and column_name=' + QuotedStrW(AColName, '''') + ';'
else
  ACmdText := 'select * from information_schema.columns where table_name=' +
    QuotedStrW(ASchema, '''') + ' and column_name=' +
    QuotedStrW(AColName, '''') + ';';
Result := RecordExists(ACmdText);
end;

procedure TQProvider.CommitTrans;
begin
Dec(FTransactionLevel);
end;

constructor TQProvider.Create(AOwner: TComponent);
begin
inherited;
FParams := TStringList.Create;
FParams.Delimiter := ' ';
FDataSets := TQDataSetList.Create;
FCached := TQDataSetList.Create;
FCommandTimeout := 30;
FPeekInterval := 60;
end;

destructor TQProvider.Destroy;

var
  I: Integer;
begin
Close;
for I := 0 to FCached.Count - 1 do
  FreeObject(FCached[I]);
FreeObject(FParams);
FreeObject(FDataSets);
FreeObject(FCached);
inherited;
end;

procedure TQProvider.DoLivePeek(AJob: PQJob);
begin
if Connected and (not Peeking) then
  begin
  SetFlags(PF_PEEKING, True);
  try
    ExecuteCmd('select 0 as qdac_peek;');
  finally
    SetFlags(PF_PEEKING, False);
  end;
  end;
end;

procedure TQProvider.DoParamChanged;
begin
if Assigned(FOnParamChanged) then
  FOnParamChanged(Self);
end;

function TQProvider.Execute(var ARequest: TQSQLRequest): THandle;
var
  AResult: TQExecuteResult;
begin
if not Connected then
  DatabaseError(SNotConnected);
if (not Peeking) and Assigned(FBeforeExecute) then
  FBeforeExecute(Self, ARequest.Command);
try
  if Assigned(FAfterExecute) then // 指定了AfterExecute才有必要计时
    ARequest.Result.Statics.ExecuteStartTime := GetTimeStamp;
  Result := InternalExecute(ARequest);
finally
  if ARequest.WaitResult then
    begin
    if Assigned(FAfterExecute) then
      begin
      ARequest.Result.Statics.ExecuteDoneTime := GetTimeStamp;
      FAfterExecute(Self, ARequest.Command, ARequest.Result);
      end;
    end
end;
end;

function TQProvider.ExecuteCmd(ACmdText: QStringW;
  const AParams: array of const): Integer;
var
  ARequest: TQSQLRequest;
begin
try
  InitializeRequest(ARequest, ACmdText, True);
  InternalSetParams(ARequest.Command.Params, AParams);
  if Execute(ARequest) >= 0 then
    Result := ARequest.Result.Statics.AffectRows
  else
    Result := -1;
finally
  FreeObject(ARequest.Command.Params);
end;
end;

function TQProvider.ExecuteCmd(AParams: TQCommand): Integer;
var
  ARequest: TQSQLRequest;
begin
InitializeRequest(ARequest, '', False);
ARequest.Command := AParams;
if Execute(ARequest) >= 0 then
  Result := ARequest.Result.Statics.AffectRows
else
  Result := -1;
end;

function TQProvider.ExecuteCmd(ACmdText: QStringW): Integer;
var
  ARequest: TQSQLRequest;
begin
if Length(ACmdText) > 0 then
  begin
  InitializeRequest(ARequest, ACmdText, False);
  if Execute(ARequest) >= 0 then
    Result := ARequest.Result.Statics.AffectRows
  else
    Result := -1;
  end
else
  DatabaseError(SEmptySQL);
end;

function TQProvider.FunctionExists(AName: QStringW): Boolean;
var
  ACmdText: QStringW;
  ASchema: QStringW;
  pName: PQCharW;
begin
pName := PWideChar(AName);
ASchema := DecodeTokenW(pName, '.', '"', True);
AName := pName;
if Length(AName) > 0 then
  ACmdText := 'select * from information_schema.routines where routine_schema='
    + QuotedStrW(ASchema, '''') + ' and routine_name=' + QuotedStrW(AName, '''')
    + ' and routine_type=''FUNCTION'';'
else
  ACmdText := 'select * from information_schema.routines where routine_name=' +
    QuotedStrW(ASchema, '''') + ' and routine_type=''FUNCTION'';';
Result := RecordExists(ACmdText);
end;

function TQProvider.GetConnected: Boolean;
begin
Result := (FHandle <> 0);
end;

function TQProvider.GetConnectionString: QStringW;
begin
Result := FParams.DelimitedText;
end;

function TQProvider.GetFlags(const Index: Integer): Boolean;
begin
Result := (FFlags and Index) <> 0;
end;

function TQProvider.GetParams: TStrings;
begin
Result := FParams;
end;

procedure TQProvider.InitializeRequest(var ARequest: TQSQLRequest;
  const ASQL: QStringW; ACreateParams: Boolean);
begin
ARequest.Command.SQL := ASQL;
if ACreateParams then
  ARequest.Command.Params := TParams.Create
else
  ARequest.Command.Params := nil;
ARequest.WaitResult := True;
FillChar(ARequest.Result.Statics, Sizeof(TQExecuteStatics), 0);
ARequest.Result.Statics.QueuedTime := GetTimeStamp;
ARequest.AfterOpen := nil;
ARequest.Result.ErrorCode := 0;
SetLength(ARequest.Result.ErrorMsg, 0);
end;

procedure TQProvider.InternalApplyUpdate(ADataSource: IQDataProducer);
var
  ACount, I, J, AColCount: Integer;
  ARequests: array [TUpdateStatus] of TQSQLRequest;
  AInited: array [TUpdateStatus] of Boolean;
  ARecord: IQRecord;
  AStatus: TUpdateStatus;
begin
ACount := ADataSource.GetChangedCount;
try
  AInited[usModified] := False;
  AInited[usInserted] := False;
  AInited[usDeleted] := False;
  // Todo:应该生成一个批量请求，然后批量提交到服务器端执行
  while I < ACount do
    begin
    ARecord := ADataSource.GetChanges(I);
    AStatus := ARecord.GetUpdateStatus;
    if AInited[AStatus] then
      begin
      if not PrepareChangeRequest(ARequests[AStatus], AStatus) then
        DatabaseError('无法生成更新需要的脚本对象');
      end;
    // 变更和删除的处理
    end;
finally
  if Assigned(ARequests[usInserted].Command.Params) then
    FreeObject(ARequests[usInserted].Command.Params);
  if Assigned(ARequests[usModified].Command.Params) then
    FreeObject(ARequests[usModified].Command.Params);
  if Assigned(ARequests[usDeleted].Command.Params) then
    FreeObject(ARequests[usDeleted].Command.Params);
end;
end;

procedure TQProvider.InternalApplyUpdates(ARootField: TQField;
  ARecords: TQRecords);
begin
if Length(Name) > 0 then
  DatabaseError(Format(SUpdateNotSupport, [Name]))
else
  DatabaseError(Format(SUpdateNotSupport, [ProviderName]))
end;

procedure TQProvider.InternalSetParams(ADest: TParams;
  const ASource: array of const);
var
  I, L, H: Integer;
  procedure AsVariant(AParam: TParam; const AValue: Variant);
  var
    J, AL, AH: Integer;
    ASubParams: TParams;
  begin
  if VarIsArray(AValue) then
    begin
    ASubParams := TParams.Create;
    AParam.AsParams := ASubParams;
    AL := VarArrayLowBound(AValue, VarArrayDimCount(AValue));
    AH := VarArrayHighBound(AValue, VarArrayDimCount(AValue));
    for J := AL to AH do
      AsVariant(ASubParams.AddParameter, AValue[J]);
    end
  else
    begin
    case VarType(AValue) of
      varEmpty, varNull, varUnknown:
        AParam.Clear;
      varSmallInt:
        AParam.AsSmallint := AValue;
      varInteger:
        AParam.AsInteger := AValue;
      varByte:
        AParam.AsByte := AValue;
      varShortInt:
        AParam.AsShortint := AValue;
      varWord:
        AParam.AsWord := AValue;
      varLongWord:
        AParam.AsLongWord := AValue;
      varInt64:
        AParam.AsLargeInt := AValue;
      varSingle:
        AParam.AsSingle := AValue;
      varDouble:
        AParam.AsFloat := AValue;
      varCurrency:
        AParam.AsCurrency := AValue;
      varDate:
        AParam.AsDateTime := AValue;
      varOleStr, varString{$IFDEF UNICODE}, varUString{$ENDIF}:
        AParam.AsWideString := AValue;
      varBoolean:
        AParam.AsBoolean := AValue;
{$IF RtlVersion>=26}
      varUInt64:
        AParam.AsLargeInt := AValue;
      varRecord:
        raise Exception.Create(SUnsupportParamValue);
{$IFEND >=XE5}
    end;
    end;
  end;

begin
L := Low(ASource);
H := High(ASource);
for I := L to H do
  begin
  case ASource[I].VType of
    vtInteger:
      ADest.AddParameter.AsInteger := ASource[I].VInteger;
    vtBoolean:
      ADest.AddParameter.AsBoolean := ASource[I].VBoolean;
{$IFNDEF NEXTGEN}
    vtChar:
      ADest.AddParameter.AsWideString := QStringW(ASource[I].VChar);
{$ENDIF !NEXTGEN}
    vtExtended:
      ADest.AddParameter.AsFloat := ASource[I].VExtended^;
{$IFNDEF NEXTGEN}
    vtPChar:
      ADest.AddParameter.AsWideString := QStringW(ASource[I].VPChar);
    vtString:
      ADest.AddParameter.AsWideString := ASource[I].VString^;
    vtAnsiString:
      ADest.AddParameter.AsWideString :=
{$IFDEF UNICODE}PAnsiString(ASource[I].VAnsiString)^{$ELSE}ASource[I]
        .VPChar{$ENDIF};
    vtWideString:
      ADest.AddParameter.AsWideString := PWideString(ASource[I].VWideString)^;
{$ENDIF !NEXTGEN}
    vtWideChar:
      ADest.AddParameter.AsWideString := ASource[I].VWideChar;
    vtPWideChar:
      ADest.AddParameter.AsWideString := ASource[I].VPWideChar;
    vtCurrency:
      ADest.AddParameter.AsCurrency := ASource[I].VCurrency^;
    vtInt64:
      ADest.AddParameter.AsLargeInt := ASource[I].VInt64^;
{$IFDEF UNICODE}       // variants
    vtUnicodeString:
      ADest.AddParameter.AsWideString := ASource[I].VPWideChar;
{$ENDIF UNICODE}
    vtVariant:
      AsVariant(ADest.AddParameter, ASource[I].VVariant^);
    vtObject:
      ADest.AddParameter.AsObject := TObject(ASource[I].VObject);
  else
    // vtPointer对于参数类型不支持
    raise Exception.Create(SUnsupportParamValue);
  end; // End case
  end; // End for
end;

procedure TQProvider.KeepAliveNeeded;
begin
// Do Nothing
end;

procedure TQProvider.Loaded;
begin
inherited;
if ((FFlags and PF_CONNECTED) <> 0) and (not Connected) then
  Open;
end;

procedure TQProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
if AComponent is TQDataSet then
  FDataSets.Remove(AComponent as TQDataSet);
inherited;
end;

function TQProvider.Open: Boolean;
begin
if not Connected then
  begin
  SetFlags(PF_CONNECTING, True);
  if Assigned(BeforeConnect) then
    BeforeConnect(Self);
  try
    InternalOpen;
    if Assigned(AfterConnected) and Connected then
      AfterConnected(Self);
  except
    on E: Exception do
      SetError(Cardinal(-1), E.Message);
  end;
  SetFlags(PF_CONNECTING, False);
  end;
if Connected then
  begin
  SetFlags(PF_CONNECTED, True);
  Result := Connected;
  end
else
  Result := False;
end;

function TQProvider.OpenDataSet(ADataSet: TQDataSet; ACmdText: QStringW;
  AfterOpen: TQAfterExecuteEvent): Boolean;
var
  ARequest: TQSQLRequest;
begin
InitializeRequest(ARequest, ACmdText, False);
Result := False;
ARequest.Command.DataObject := ADataSet;
ARequest.AfterOpen := AfterOpen;
if Assigned(AfterOpen) then
  ARequest.WaitResult := True;
if Execute(ARequest) >= 0 then;
begin
if FDataSets.IndexOf(ADataSet) = -1 then
  begin
  ADataSet.FreeNotification(Self);
  FDataSets.Add(ADataSet);
  end;
Result := True;
end;
end;

function TQProvider.OpenDataSet(ACmdText: QStringW): TQDataSet;
begin
Result := AcquireDataSet;
if not OpenDataSet(Result, ACmdText) then
  begin
  ReleaseDataSet(Result);
  Result := nil;
  end;
end;

function TQProvider.OpenDataSet(ACmdText: QStringW; AParams: array of const)
  : TQDataSet;
begin

end;

function TQProvider.OpenDataSet(ADataSet: TQDataSet; ACmdText: QStringW;
  AParams: array of const; AfterOpen: TQAfterExecuteEvent): Boolean;
begin

end;

function TQProvider.OpenDataSet(ADataSet: TQDataSet; ASQL: TQCommand;
  AfterOpen: TQAfterExecuteEvent): Boolean;
begin

end;

function TQProvider.OpenStream(ACmdText: QStringW;
  AStreamFormat: TQConverterClass): TMemoryStream;
// var
// AStream: TMemoryStream;
// AHandle: THandle;
// AConverter: TQConverter;
begin
// Result := nil;
// AHandle := Execute(ACmdText, TQDataSet(-1));
// if AHandle <> 0 then
// begin
// AStream := TMemoryStream.Create;
// AConverter := AStreamFormat.Create(Self);
// try
// AConverter.SaveToStream(Self, AHandle, AStream);
// Result := AStream;
// except
// AStream.Free;
// end;
// AConverter.Free;
// DestroyHandle(AHandle);
// end;
end;

function TQProvider.OpenStream(ASQL: TQCommand; AStreamFormat: TQConverterClass)
  : TMemoryStream;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ACmdText: QStringW;
  AStreamFormat: TQConverterClass; AParams: array of const): Boolean;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ASQL: TQCommand;
  AStreamFormat: TQConverterClass): Boolean;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ACmdText: QStringW;
  AStreamFormat: TQConverterClass): Boolean;
begin

end;

function TQProvider.OpenStream(ACmdText: QStringW;
  AStreamFormat: TQConverterClass; AParams: array of const): TMemoryStream;
begin

end;

function TQProvider.OpenStream(ASQL: TQCommand; AStreamFormat: TQConverter)
  : TMemoryStream;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ACmdText: QStringW;
  AStreamFormat: TQConverter; AParams: array of const): Boolean;
begin

end;

function TQProvider.OpenStream(ACmdText: QStringW; AStreamFormat: TQConverter;
  AParams: array of const): TMemoryStream;
begin

end;

function TQProvider.OpenStream(ACmdText: QStringW; AStreamFormat: TQConverter)
  : TMemoryStream;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ACmdText: QStringW;
  AStreamFormat: TQConverter): Boolean;
begin

end;

function TQProvider.OpenStream(AStream: TStream; ASQL: TQCommand;
  AStreamFormat: TQConverter): Boolean;
begin

end;

function TQProvider.Prepare(ACmdText, AName: QStringW): TQCommand;
begin

end;

function TQProvider.PrepareChangeRequest(var ARequest: TQSQLRequest;
  AUpdatStatus: TUpdateStatus): Boolean;
begin
Result := False;
end;

function TQProvider.ProcedureExists(AName: QStringW): Boolean;
var
  ACmdText: QStringW;
  ASchema: QStringW;
  pName: PQCharW;
begin
pName := PQCharW(AName);
ASchema := DecodeTokenW(pName, '.', '"', True);
AName := pName;
if Length(AName) > 0 then
  ACmdText := 'select * from information_schema.routines where routine_schema='
    + QuotedStrW(ASchema, '''') + ' and routine_name=' + QuotedStrW(AName, '''')
    + ' and routine_type=''PROCEDURE'';'
else
  ACmdText := 'select * from information_schema.routines where routine_name=' +
    QuotedStrW(ASchema, '''') + ' and routine_type=''PROCEDURE'';';
Result := RecordExists(ACmdText);
end;

function TQProvider.RecordExists(ACmdText: QStringW): Boolean;
begin
if not Connected then
  DatabaseError(SNotConnected);
Result := False;
Result := (ExecuteCmd(ACmdText) > 0);
end;

procedure TQProvider.ReleaseDataSet(ADataSet: TQDataSet);
begin
if not Assigned(ADataSet.Owner) then
  begin
  ADataSet.Close;
  FCached.Add(ADataSet)
  end
else
  FreeObject(ADataSet);
end;

procedure TQProvider.RollbackTrans(ASavePointName: QStringW);
begin
Dec(FTransactionLevel);
end;

procedure TQProvider.SetConnected(const Value: Boolean);
begin
if csLoading in ComponentState then
  SetFlags(PF_CONNECTED, Value)
else if Value then
  begin
  if not Connected then
    begin
    if not Open then
      DatabaseError(Format(SCantConnectToDB, [LastError, LastErrorMsg]));
    end;
  end
else
  begin
  Close;
  end;
end;

procedure TQProvider.SetConnectionString(const Value: QStringW);
var
  ALastConnected: Boolean;
begin
if FConnectionString <> Value then
  begin
  ALastConnected := Connected;
  if ALastConnected then
    Close;
  FParams.DelimitedText := Value;
  DoParamChanged;
  if ALastConnected then
    Open;
  end;
end;

procedure TQProvider.SetError(ACode: Cardinal; const AMsg: QStringW);
begin
FErrorCode := ACode;
FErrorMsg := AMsg;
end;

procedure TQProvider.SetFlags(AFlag: Integer; AValue: Boolean);
begin
if AValue then
  FFlags := FFlags or AFlag
else
  FFlags := FFlags and (not AFlag);
end;

procedure TQProvider.SetKeepAlive(const Value: Boolean);
begin
if KeepAlive <> Value then
  begin
  SetFlags(PF_KEEPALIVE, Value);
  if Value and Connected then
    KeepAliveNeeded;
  end;
end;

procedure TQProvider.SetParams(const Value: TStrings);
begin
FParams.Assign(Value);
DoParamChanged;
end;

procedure TQProvider.SetPeekInterval(const Value: Integer);
begin
if (Value <> FPeekInterval) and (Value > 0) then
  begin
  FPeekInterval := Value;
  if KeepAlive and Connected then
    begin
    Workers.Clear;
    Workers.Post(DoLivePeek, PeekInterval * 10, nil, True);
    end;
  end;
end;

function TQProvider.TableExists(ATableName: QStringW): Boolean;
var
  ACmdText: QStringW;
  ASchema: QStringW;
  pName: PQCharW;
begin
pName := PWideChar(ATableName);
ASchema := DecodeTokenW(pName, '.', '"', True);
ATableName := pName;
if Length(ATableName) > 0 then
  ACmdText := 'select * from information_schema.tables where table_schema=' +
    QuotedStrW(ASchema, '''') + ' and table_name=' + QuotedStrW(ATableName,
    '''') + ' and table_type=''BASE TABLE'''
else
  ACmdText := 'select * from information_schema.tables where table_name=' +
    QuotedStrW(ASchema, '''') + ' and table_type=''BASE TABLE''';
Result := RecordExists(ACmdText);
end;

function TQProvider.TriggerExists(AName: QStringW): Boolean;
var
  ACmdText: QStringW;
  ASchema: QStringW;
  pName: PQCharW;
begin
pName := PWideChar(AName);
ASchema := DecodeTokenW(pName, '.', '"', True);
AName := pName;
if Length(AName) > 0 then
  ACmdText := 'select * from information_schema.triggers where trigger_schema='
    + QuotedStrW(ASchema, '''') + ' and trigger_name=' +
    QuotedStrW(AName, '''') + ';'
else
  ACmdText := 'select * from information_schema.triggers where trigger_name=' +
    QuotedStrW(ASchema, '''') + ';';
Result := RecordExists(ACmdText);
end;

function TQProvider.ViewExists(AName: QStringW): Boolean;
var
  ACmdText: QStringW;
  ASchema: QStringW;
  pName: PQCharW;
begin
pName := PQCharW(AName);
ASchema := DecodeTokenW(pName, '.', '"', True);
AName := pName;
if Length(AName) > 0 then
  ACmdText := 'select * from information_schema.views where table_schema=' +
    QuotedStrW(ASchema, '''') + ' and table_name=' +
    QuotedStrW(AName, '''') + ';'
else
  ACmdText := 'select * from information_schema.views where table_name=' +
    QuotedStrW(ASchema, '''') + ';';
Result := RecordExists(ACmdText);
end;

{ TQDataSet }

function TQDataSet.AddRecord(ARecord: IQRecord): Boolean;
begin

end;

function TQDataSet.AllocRecord: IQRecord;
begin

end;

procedure TQDataSet.FetchAllRecords;
begin

end;

function TQDataSet.GetChangedCount: Integer;
begin

end;

function TQDataSet.GetChanges(AIndex: Integer): IQRecord;
begin

end;

function TQDataSet.GetRecordCount: Integer;
begin

end;

function TQDataSet.GetRecords(AIndex: Integer): IQRecord;
begin

end;

function TQDataSet.GetRootField: IQField;
begin
Result := FRootField;
end;

procedure TQDataSet.SetProvider(const Value: TQProvider);
begin
FProvider := Value;
end;

{ TQConverter }

function TQConverter.AddRecord(ARecord: IQRecord): Boolean;
begin

end;

procedure TQConverter.AfterExport;
begin

end;

procedure TQConverter.AfterImport;
begin

end;

function TQConverter.AllocRecord: IQRecord;
begin

end;

procedure TQConverter.BeforeExport;
begin

end;

procedure TQConverter.BeforeImport;
begin

end;

constructor TQConverter.Create(AOwner: TComponent);
begin
inherited;

end;

destructor TQConverter.Destroy;
begin

inherited;
end;

function TQConverter.GetChangedCount: Integer;
begin

end;

function TQConverter.GetChanges(AIndex: Integer): IQRecord;
begin

end;

function TQConverter.GetRecordCount: Integer;
begin

end;

function TQConverter.GetRecords(AIndex: Integer): IQRecord;
begin

end;

function TQConverter.GetRootField: IQField;
begin

end;

procedure TQConverter.LoadFromFile(ADataSet: TQDataSet; AFileName: WideString);
begin

end;

procedure TQConverter.LoadFromStream(ADataSet: TQDataSet; AStream: TStream);
begin

end;

procedure TQConverter.SaveToFile(ADataSet: TQDataSet; AFileName: WideString);
begin

end;

procedure TQConverter.SaveToStream(AProvider: TQProvider; AResult: THandle;
  AStream: TStream);
begin

end;

procedure TQConverter.SaveToStream(ADataSet: TQDataSet; AStream: TStream);
begin

end;

end.
