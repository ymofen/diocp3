unit qworker;
{$I 'qdac.inc'}

interface

// 在线程数太多时TQSimpleLock的自旋带来过多的开销，反而不如临界，所以暂时放弃使用
{ .$DEFINE QWORKER_SIMPLE_LOCK }
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
  2014.8.19
  =========
  * 修正了TQJob.Synchronize由于inline声明造成在2007下无法正确编译的问题
  2014.8.18
  =========
  * 修正了合并代码造成LongTimeJob投寄数量限制错误的问题(志文报告）
  * 修正了昨天残留的TQJobGroup.Run函数超时设置出错的问题
  + TQJobGroup增加MsgWaitFor函数，以便在主线程中等待而不阻塞主线程(麦子仲肥测试验证)
  + TQJob增加Synchronize函数，实际上公开的是TThread.Synchronize方法(麦子仲肥测试验证)

  2014.8.17
  =========
  * 改进查找空闲线程机制，以避免不必要开销（感谢音儿小白和笑看红尘)
  * 合并代码，以减少重复代码量（感谢音儿小白）
  * 更改了Wait函数接口，AData和AFreeType参数被取消，改为在信号触发时传递相关参数
  * TQJobGroup.AfterDone改为除了在完成时，在中断或超时时仍然触发
  + TQJobGroup.Add函数加入了AFreeType参数
  + TQJobGroup.Run函数加入超时设置，超过指定的时间如果仍未执行完成，则中止后续执行(Bug出没，请注意未彻底搞定)
  + TQJobGroup.Cancel函数用于取消未执行的作业执行

  2014.8.14
  ==========
  * 参考音儿小白的建议，修改Assign函数，同时TQJobHelper的多个属性改为使用同一个函数实现
  * 修正了在Delphi2007上编译的问题(音儿小白报告并提供修改)
  2014.8.12
  ==========
  * 修正了TQJob.Assign函数忘记复制WorkerProcA成员的问题
  2014.8.8
  ==========
  * 修正了在主线程中Clear时，如果有主线程的作业已投寄到主线程消息队列但尚未执行时
  会出现锁死的问题(playwo报告)

  2014.8.7
  ==========
  * 修正了TQJobGroup添加作业时，忘记修改作业完成状态的问题

  2014.8.2
  ==========
  * 修正了在Windows下DLL中使用QWorker时，由于退出时，线程异常中止时，程序无法退
  出的问题(小李飞刀报告，天地弦验证)
  2014.7.29
  ==========
  + 添加了匿名和全局函数重载形式，在XE5以上版本中，可以支持匿名函数做为作业过程
  [注意]匿名函数不应访问局部变量的值
  2014.7.28
  ==========
  * 修正了ComNeeded函数忘记设置初始化完成标志位的问题(天地弦报告)
  2014.7.21
  ==========
  * 修正了Delphi 2007无法编译的问题

  2014.7.17
  =========
  * 修正了在FMX平台上编译时引用Hint的代码
  2014.7.14
  =========
  * 修正了TQJobGroup没有触发AfterDone事件的问题
  * 修改了引发Hint的代码
  2014.7.12
  =========
  + 添加TQJobGroup支持作业分组
  2014.7.4
  ========
  * 修正了与FMX的兼容性问题(恢弘报告)
  + 加入Clear的清除全部作业的重载实现(D10天地弦建议)
  * 支持在作业过程中通过设置IsTerminated属性来安全结束定时及信号作业
  2014.7.3
  =========
  + MakeJobProc来支持全局作业处理函数
  + TQWorkers.Clear函数增加了两个重载函数，实现清理指定信号关联的全部作业(五毒公主ら。建议)
  * 修正了重复作业正在执行时无法清除干净的问题
  2014.6.26
  =========
  * TEvent.WaitFor加入参数，以解决与Delphi2007的兼容性(D10-天地弦报告)
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
  2014.6.23
  =========
  * 修改了Windows下主线程中作业的触发方式，以改善与COM的兼容性（D10-天地弦报告）
  2014.6.21
  =========
  * 加入了对COM的支持，如果需要在作业中使用COM对象，调用Job.Worker.ComNeeded后即可
  正常访问各个COM对象
  2014.6.19
  =========
  * 修正了DoMainThreadWork函数的参数传递顺序错误
  * 为TQWorker加入了ComNeeded函数，以支持COM的初始化，保障作业内COM相关函数调用
  2014.6.17
  =========
  * 信号触发作业时，加入附加数据成员参数，它将被附加到TQJob结构的Data成员，以便
  上层应用能够做必要的标记，默认值为空
  * 作业投寄时加入了附加的参数，决定如何释放附加的数据对象
}
uses
  classes, types, sysutils, SyncObjs
{$IFDEF UNICODE}, Generics.Collections{$ENDIF}
{$IFDEF NEXTGEN}, fmx.Forms, system.Diagnostics{$ENDIF}
{$IFDEF POSIX}, Posix.Unistd, Posix.Pthread{$ENDIF}
{$IFDEF MSWINDOWS}, Windows, Messages, Forms, activex{$ENDIF}
    , qstring, qrbtree;
{$HPPEMIT '#pragma link "qworker"'}

{ *QWorker是一个后台工作者管理对象，用于管理线程的调度及运行。在QWorker中，最小的
  工作单位被称为作业（Job），作业可以：
  1、在指定的时间点自动按计划执行，类似于计划任务，只是时钟的分辨率可以更高
  2、在得到相应的信号时，自动执行相应的计划任务
  【限制】
  1.时间间隔由于使用0.1ms为基本单位，因此，64位整数最大值为9223372036224000000，
  除以864000000后就可得结果约为10675199116天，因此，QWorker中的作业延迟和定时重复
  间隔最大为10675199116天。
  2、最少工作者数为1个，无论是在单核心还是多核心机器上，这是最低限制。你可以
  设置的最少工作者数必需大于等于1。工作者上限没做实际限制。
  3、长时间作业数量不得超过最多工作者数量的一半，以免影响正常普通作业的响应。因此
  投寄长时间作业时，应检查投寄结果以确认是否投寄成功
  * }
const
  JOB_RUN_ONCE = $0001; // 作业只运行一次
  JOB_IN_MAINTHREAD = $0002; // 作业只能在主线程中运行
  JOB_MAX_WORKERS = $0004; // 尽可能多的开启可能的工作者线程来处理作业，暂不支持
  JOB_LONGTIME = $0008; // 作业需要很长的时间才能完成，以便调度程序减少它对其它作业的影响
  JOB_SIGNAL_WAKEUP = $0010; // 作业根据信号需要唤醒
  JOB_TERMINATED = $0020; // 作业不需要继续进行，可以结束了
  JOB_FREE_OBJECT = $0040; // Data关联的是Object，作业完成或清理时释放
  JOB_FREE_RECORD = $0080; // Data关联的是Record，作业完成或清理时释放
  JOB_FREE_INTERFACE = $0100; // Data关联的是Interface，作业完成时调用_Release
  JOB_DATA_OWNER = $01C0; // 作业是Data成员的所有者
  JOB_GROUPED = $0200; // 当前作业是作业组的一员
  JOB_ANONPROC = $0400; // 当前作业过程是匿名函数
  WORKER_ISBUSY = $01; // 工作者忙碌
  WORKER_PROCESSLONG = $02; // 当前处理的一个长时间作业
  WORKER_RESERVED = $04; // 当前工作者是一个保留工作者
  WORKER_COM_INITED = $08; // 工作者已初始化为支持COM的状态(仅限Windows)
  WORKER_LOOKUP = $10; // 工作者正在查找作业
  WORKER_EXECUTING = $20; // 工作者正在执行作业
  WORKER_EXECUTED = $40; // 工作者已经完成作业
  Q1MillSecond = 10; // 1ms
  Q1Second = 10000; // 1s
  Q1Minute = 600000; // 60s/1min
  Q1Hour = 36000000; // 3600s/60min/1hour
  Q1Day = Int64(864000000); // 1day
{$IFNDEF UNICODE}
  wrIOCompletion = TWaitResult(4);
{$ENDIF}

type
  TQJobs = class;
  TQWorker = class;
  TQWorkers = class;
  TQJobGroup = class;
  PQSignal = ^TQSignal;
  PQJob = ^TQJob;
  /// <summary>作业处理回调函数</summary>
  /// <param name="AJob">要处理的作业信息</param>
  TQJobProc = procedure(AJob: PQJob) of object;
  TQJobProcG = procedure(AJob: PQJob);
{$IFDEF UNICODE}
  TQJobProcA = reference to procedure(AJob: PQJob);
{$ENDIF}
  /// <summary>作业空闲原因，内部使用</summary>
  /// <remarks>
  /// irNoJob : 没有需要处理的作业，此时工作者会进行15秒释放等待状态，如果在15秒内
  /// 有新作业进来，则工作者会被唤醒，否则超时后会被释放
  /// irTimeout : 工作者已经等待超时，可以被释放
  TWorkerIdleReason = (irNoJob, irTimeout);
  /// <summary>作业结束时如何处理Data成员</summary>
  /// <remarks>
  /// jdoFreeByUser : 用户管理对象的释放
  /// jdoFreeAsObject : 附加的是一个TObject继承的对象，作业完成时会调用FreeObject释放
  /// jdoFreeAsRecord : 附加的是一个Record对象，作业完成时会调用Dispose释放
  /// jdtFreeAsInterface : 附加的是一个接口对象，添加时会增加计数，作业完成时会减少计数
  /// </remarks>
  TQJobDataFreeType = (jdfFreeByUser, jdfFreeAsObject, jdfFreeAsRecord,
    jdfFreeAsInterface);

  TQJob = record
  private
    function GetAvgTime: Integer; inline;
    function GetEscapedTime: Int64; inline;
    function GetIsTerminated: Boolean; inline;
    function GetFlags(AIndex: Integer): Boolean; inline;
    procedure SetFlags(AIndex: Integer; AValue: Boolean); inline;
    procedure UpdateNextTime;
    procedure SetIsTerminated(const Value: Boolean);
    procedure AfterRun(AUsedTime: Int64);
  public
    constructor Create(AProc: TQJobProc); overload;
    /// <summary>值拷贝函数</summary>
    /// <remarks>Worker/Next/Source不会复制并会被置空，Owner不会被复制</remarks>
    procedure Assign(const ASource: PQJob);
    /// <summary>重置内容，以便为从队列中弹出做准备</summary>
    procedure Reset; inline;

    /// <summary>公开下线程对象的同步方法，但更推荐投寄异步作业到主线程中处理</summary>
    procedure Synchronize(AMethod: TThreadMethod); {$IFDEF UNICODE}inline;{$ENDIF}
    /// <summary>平均每次运行时间，单位为0.1ms</summary>
    property AvgTime: Integer read GetAvgTime;
    /// <summmary>本次已运行时间，单位为0.1ms</summary>
    property EscapedTime: Int64 read GetEscapedTime;
    /// <summary>是否只运行一次，投递作业时自动设置</summary>
    property Runonce: Boolean index JOB_RUN_ONCE read GetFlags;
    /// <summary>是否要求在主线程执行作业，实际效果比Windows的PostMessage相似</summary>
    property InMainThread: Boolean index JOB_IN_MAINTHREAD read GetFlags;
    /// <summary>是否是一个运行时间比较长的作业，用Workers.LongtimeWork设置</summary>
    property IsLongtimeJob: Boolean index JOB_LONGTIME read GetFlags;
    /// <summary>是否是一个信号触发的作业</summary>
    property IsSignalWakeup: Boolean index JOB_SIGNAL_WAKEUP read GetFlags;
    /// <summary>是否是分组作业的成员</summary>
    property IsGrouped: Boolean index JOB_GROUPED read GetFlags;
    /// <summary>是否要求结束当前作业</summary>
    property IsTerminated: Boolean read GetIsTerminated write SetIsTerminated;
    /// <summary>判断作业的Data指向的是一个对象且要求作业完成时自动释放</summary>
    property IsObjectOwner: Boolean index JOB_FREE_OBJECT read GetFlags
      write SetFlags;
    /// <summary>判断作业的Data指向的是一个记录且要求作业完成时自动释放</summary>
    property IsRecordOwner: Boolean index JOB_FREE_RECORD read GetFlags
      write SetFlags;
    /// <summary>判断作业是否拥有Data数据成员
    property IsDataOwner: Boolean index JOB_DATA_OWNER read GetFlags;
    /// <summary>判断作业的Data指向的是一个接口且要求作业完成时自动释放</summary>
    property IsInterfaceOwner: Boolean index JOB_FREE_INTERFACE read GetFlags
      write SetFlags;
    /// <summary>判断作业处理过程是否是一个匿名函数</summary>
    property IsAnonWorkerProc: Boolean index JOB_ANONPROC read GetFlags
      write SetFlags;
  public
    FirstStartTime: Int64; // 作业第一次开始时间
    StartTime: Int64; // 本次作业开始时间,8B
    PushTime: Int64; // 入队时间
    PopTime: Int64; // 出队时间
    NextTime: Int64; // 下一次运行的时间,+8B=16B
    WorkerProc: TQJobProc; // 作业处理函数+8/16B
{$IFDEF UNICODE}
    WorkerProcA: TQJobProcA;
{$ENDIF}
    Owner: TQJobs; // 作业所隶属的队列
    Next: PQJob; // 下一个结点
    Worker: TQWorker; // 当前作业工作者
    Runs: Integer; // 已经运行的次数+4B
    MinUsedTime: Integer; // 最小运行时间+4B
    TotalUsedTime: Integer; // 运行总计花费的时间，TotalUsedTime/Runs可以得出平均执行时间+4B
    MaxUsedTime: Integer; // 最大运行时间+4B
    Flags: Integer; // 作业标志位+4B
    Data: Pointer; // 附加数据内容
    case Integer of
      0:
        (SignalId: Integer; // 信号编码
          Source: PQJob; // 源作业地址
          RefCount: PInteger; // 源数据
        );
      1:
        (Interval: Int64; // 运行时间间隔，单位为0.1ms，实际精度受不同操作系统限制+8B
          FirstDelay: Int64; // 首次运行延迟，单位为0.1ms，默认为0
        );
      2: // 分组作业支持
        (Group: Pointer;
        );
  end;

  // /// <summary>工作者记录的辅助函数</summary>
  // TQJobHelper = record helper for TQJob
  //
  // end;

  // 作业队列对象的基类，提供基础的接口封装
  TQJobs = class
  protected
    FOwner: TQWorkers;
    function InternalPush(AJob: PQJob): Boolean; virtual; abstract;
    function InternalPop: PQJob; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetEmpty: Boolean;
    /// <summary>投寄一个作业</summary>
    /// <param name="AJob">要投寄的作业</param>
    /// <remarks>外部不应尝试直接投寄任务到队列，其由TQWorkers的相应函数内部调用。</remarks>
    function Push(AJob: PQJob): Boolean; virtual;
    /// <summary>弹出一个作业</summary>
    /// <returns>返回当前可以执行的第一个作业</returns>
    function Pop: PQJob; virtual;
    /// <summary>清空所有作业</summary>
    procedure Clear; overload; virtual;
    /// <summary>清空指定的作业</summary>
    function Clear(AProc: TQJobProc; AData: Pointer; AMaxTimes: Integer)
      : Integer; overload; virtual; abstract;
    /// <summary>清空一个对象关联的所有作业</summary>
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer; overload;
      virtual; abstract;
  public
    constructor Create(AOwner: TQWorkers); overload; virtual;
    destructor Destroy; override;
    /// 不可靠警告：Count和Empty值仅是一个参考，在多线程环境下可能并不保证下一句代码执行时，会一致
    property Empty: Boolean read GetEmpty; // 当前队列是否为空
    property Count: Integer read GetCount; // 当前队列元素数量
  end;
{$IFDEF QWORKER_SIMPLE_LOCK}

  // 一个基于位锁的简单锁定对象，使用原子函数置位
  TQSimpleLock = class
  private
    FFlags: Integer;
  public
    constructor Create;
    procedure Enter; inline;
    procedure Leave; inline;
  end;
{$ELSE}

  TQSimpleLock = TCriticalSection;
{$ENDIF}

  // TQSimpleJobs用于管理简单的异步调用，没有触发时间要求的作业
  TQSimpleJobs = class(TQJobs)
  protected
    FFirst, FLast: PQJob;
    FCount: Integer;
    FLocker: TQSimpleLock;
    function InternalPush(AJob: PQJob): Boolean; override;
    function InternalPop: PQJob; override;
    function GetCount: Integer; override;
    procedure Clear; overload; override;
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
      overload; override;
    function Clear(AProc: TQJobProc; AData: Pointer; AMaxTimes: Integer)
      : Integer; overload; override;
  public
    constructor Create(AOwner: TQWorkers); override;
    destructor Destroy; override;
  end;

  // TQRepeatJobs用于管理计划型任务，需要在指定的时间点触发
  TQRepeatJobs = class(TQJobs)
  protected
    FItems: TQRBTree;
    FLocker: TCriticalSection;
    FFirstFireTime: Int64;
    function InternalPush(AJob: PQJob): Boolean; override;
    function InternalPop: PQJob; override;
    function DoTimeCompare(P1, P2: Pointer): Integer;
    procedure DoJobDelete(ATree: TQRBTree; ANode: TQRBNode);
    function GetCount: Integer; override;
    procedure Clear; override;
    function Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
      overload; override;
    function Clear(AProc: TQJobProc; AData: Pointer; AMaxTimes: Integer)
      : Integer; overload; override;
    procedure AfterJobRun(AJob: PQJob; AUsedTime: Int64);
  public
    constructor Create(AOwner: TQWorkers); override;
    destructor Destroy; override;
  end;

  { 工作者线程使用单向链表管理，而不是进行排序检索是因为对于工作者数量有限，额外
    的处理反而不会直接最简单的循环直接有效
  }
  TQWorker = class(TThread)
  protected
    FOwner: TQWorkers;
    FEvent: TEvent;
    FTimeout: Integer;
    FNext: TQWorker;
    FFlags: Integer;
    FActiveJob: PQJob;
    // 之所以不直接使用FActiveJob的相关方法，是因为保证外部可以线程安全的访问这两个成员
    FActiveJobProc: TQJobProc;
    FActiveJobData: Pointer;
    FActiveJobSource: PQJob;
    FActiveJobGroup: TQJobGroup;
    FActiveJobFlags: Integer;
    FTerminatingJob: PQJob;
    procedure Execute; override;
    procedure FireInMainThread;
    procedure DoJob(AJob: PQJob);
    function GetIsIdle: Boolean; inline;
    procedure SetFlags(AIndex: Integer; AValue: Boolean); inline;
    function GetFlags(AIndex: Integer): Boolean; inline;
  public
    constructor Create(AOwner: TQWorkers); overload;
    destructor Destroy; override;
    procedure ComNeeded(AInitFlags: Cardinal = 0);
    /// <summary>判断当前是否处于长时间作业处理过程中</summary>
    property InLongtimeJob: Boolean index WORKER_PROCESSLONG read GetFlags;
    /// <summary>判断当前是否空闲</summary>
    property IsIdle: Boolean read GetIsIdle;
    /// <summary>判断当前是否忙碌</summary>
    property IsBusy: Boolean index WORKER_ISBUSY read GetFlags;
    /// <summary>判断当前工作者是否是内部保留的工作者
    property IsReserved: Boolean index WORKER_RESERVED read GetFlags;
    property IsLookuping: Boolean index WORKER_LOOKUP read GetFlags;
    property IsExecuting: Boolean index WORKER_EXECUTING read GetFlags;
    property IsExecuted: Boolean index WORKER_EXECUTED read GetFlags;
    /// <summary>判断COM是否已经初始化为支持COM
    property ComInitialized: Boolean index WORKER_COM_INITED read GetFlags;
  end;

  /// <summary>信号的内部定义</summary>
  TQSignal = record
    Id: Integer;
    /// <summary>信号的编码</summary>
    Fired: Integer; // <summary>信号已触发次数</summary>
    Name: QStringW;
    /// <summary>信号的名称</summary>
    First: PQJob;
    /// <summary>首个作业</summary>
  end;

  TWorkerWaitParam = record
    WaitType: Byte;
    Data: Pointer;
    case Integer of
      0:
        (Bound: Pointer); // 按对象清除
      1:
        (WorkerProc: TMethod;);
      2:
        (SourceJob: PQJob);
      3:
        (Group: Pointer);
  end;

  /// <summary>工作者管理对象，用来管理工作者和作业</summary>
  TQWorkers = class
  protected
    FWorkers: array of TQWorker;
    FDisableCount: Integer;
    FMinWorkers: Integer;
    FMaxWorkers: Integer;
    FWorkerCount: Integer;
    FBusyCount: Integer;
    FLongTimeWorkers: Integer; // 记录下长时间作业中的工作者，这种任务长时间不释放资源，可能会造成其它任务无法及时响应
    FMaxLongtimeWorkers: Integer; // 允许最多同时执行的长时间任务数，不允许超过MaxWorkers的一半
    FLocker: TCriticalSection;
    FSimpleJobs: TQSimpleJobs;
    FRepeatJobs: TQRepeatJobs;
    FSignalJobs: TQHashTable;
    FMaxSignalId: Integer;
    FTerminating: Boolean;
{$IFDEF MSWINDOWS}
    FMainWorker: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
{$ENDIF}
    function Popup: PQJob;
    procedure SetMaxWorkers(const Value: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetMinWorkers(const Value: Integer);
    procedure WorkerIdle(AWorker: TQWorker; AReason: TWorkerIdleReason); inline;
    procedure WorkerBusy(AWorker: TQWorker); inline;
    procedure WorkerTerminate(AWorker: TObject);
    procedure FreeJob(AJob: PQJob);
    function LookupIdleWorker: Boolean;
    procedure ClearWorkers;
    procedure SignalWorkDone(AJob: PQJob; AUsedTime: Int64);
    procedure DoJobFree(ATable: TQHashTable; AHash: Cardinal; AData: Pointer);
    function Post(AJob: PQJob): Boolean; overload;
    procedure SetMaxLongtimeWorkers(const Value: Integer);
    function SignalIdByName(const AName: QStringW): Integer;
    procedure FireSignalJob(ASignal: PQSignal; AData: Pointer;
      AFreeType: TQJobDataFreeType);
    function ClearSignalJobs(ASource: PQJob): Integer;
    procedure WaitSignalJobsDone(AJob: PQJob);
    procedure WaitRunningDone(const AParam: TWorkerWaitParam);
    procedure FreeJobData(AData: Pointer; AFreeType: TQJobDataFreeType);
  public
    constructor Create(AMinWorkers: Integer = 2); overload;
    destructor Destroy; override;
    /// <summary>投寄一个后台立即开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProc; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个后台立即开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProcG; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个后台立即开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProcA; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个后台定时开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要定时执行的作业时间间隔，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProc; AInterval: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个后台定时开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要定时执行的作业时间间隔，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProcG; AInterval: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个后台定时开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要定时执行的作业时间间隔，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Post(AProc: TQJobProcA; AInterval: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个延迟开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要延迟的时间，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Delay(AProc: TQJobProc; ADelay: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个延迟开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要延迟的时间，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Delay(AProc: TQJobProcG; ADelay: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个延迟开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AInterval">要延迟的时间，单位为0.1ms，如要间隔1秒，则值为10000</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Delay(AProc: TQJobProcA; ADelay: Int64; AData: Pointer;
      ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个等待信号才开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="ASignalId">等待的信号编码，该编码由RegisterSignal函数返回</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Wait(AProc: TQJobProc; ASignalId: Integer;
      ARunInMainThread: Boolean = False): Boolean; overload;
    /// <summary>投寄一个等待信号才开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="ASignalId">等待的信号编码，该编码由RegisterSignal函数返回</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Wait(AProc: TQJobProcG; ASignalId: Integer;
      ARunInMainThread: Boolean = False): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个等待信号才开始的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="ASignalId">等待的信号编码，该编码由RegisterSignal函数返回</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <param name="ARunInMainThread">作业要求在主线程中执行</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    function Wait(AProc: TQJobProcA; ASignalId: Integer;
      ARunInMainThread: Boolean = False): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ADelay">第一次执行前先延迟时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProc; const ADelay, AInterval: Int64;
      AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ADelay">第一次执行前先延迟时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProcG; const ADelay, AInterval: Int64;
      AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ADelay">第一次执行前先延迟时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProcA; const ADelay, AInterval: Int64;
      AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ATime">执行时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProc; const ATime: TDateTime;
      const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ATime">执行时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProcG; const ATime: TDateTime;
      const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个在指定时间才开始的重复作业</summary>
    /// <param name="AProc">要定时执行的作业过程</param>
    /// <param name="ATime">执行时间</param>
    /// <param name="AInterval">后续作业重复间隔，如果小于等于0，则作业只执行一次，和Delay的效果一致</param>
    /// <param name="ARunInMainThread">是否要求作业在主线程中执行</param>
    function At(AProc: TQJobProcA; const ATime: TDateTime;
      const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>投寄一个后台长时间执行的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    /// <remarks>长时间作业强制在后台线程中执行，而不允许投递到主线程中执行</remarks>
    function LongtimeJob(AProc: TQJobProc; AData: Pointer;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
    /// <summary>投寄一个后台长时间执行的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    /// <remarks>长时间作业强制在后台线程中执行，而不允许投递到主线程中执行</remarks>
    function LongtimeJob(AProc: TQJobProcG; AData: Pointer;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$IFDEF UNICODE}
    /// <summary>投寄一个后台长时间执行的作业</summary>
    /// <param name="AProc">要执行的作业过程</param>
    /// <param name="AData">作业附加的用户数据指针</param>
    /// <returns>成功投寄返回True，否则返回False</returns>
    /// <remarks>长时间作业强制在后台线程中执行，而不允许投递到主线程中执行</remarks>
    function LongtimeJob(AProc: TQJobProcA; AData: Pointer;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean; overload;
{$ENDIF}
    /// <summary>清除所有作业</summary>
    procedure Clear; overload;
    /// <summary>清除一个对象相关的所有作业</summary>
    /// <param name="AObject">要释放的作业处理过程关联对象</param>
    /// <param name="AMaxTimes">最多清除的数量，如果<0，则全清</param>
    /// <returns>返回实际清除的作业数量</returns>
    /// <remarks>一个对象如果计划了作业，则在自己释放前应调用本函数以清除关联的作业，
    /// 否则，未完成的作业可能会触发异常。</remarks>
    function Clear(AObject: Pointer; AMaxTimes: Integer = -1): Integer;
      overload;
    /// <summary>清除所有投寄的指定过程作业</summary>
    /// <param name="AProc">要清除的作业执行过程</param>
    /// <param name="AData">要清除的作业附加数据指针地址，如果值为Pointer(-1)，
    /// 则清除所有的相关过程，否则，只清除附加数据地址一致的过程</param>
    /// <param name="AMaxTimes">最多清除的数量，如果<0，则全清</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(AProc: TQJobProc; AData: Pointer; AMaxTimes: Integer = -1)
      : Integer; overload;
    /// <summary>清除指定信号关联的所有作业</summary>
    /// <param name="ASingalId">要清除的信号ID</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(ASignalName: QStringW): Integer; overload;
    /// <summary>清除指定信号关联的所有作业</summary>
    /// <param name="ASingalId">要清除的信号名称</param>
    /// <returns>返回实际清除的作业数量</returns>
    function Clear(ASignalId: Integer): Integer; overload;
    /// <summary>触发一个信号</summary>
    /// <param name="AId">信号编码，由RegisterSignal返回</param>
    /// <param name="AData">附加给作业的用户数据指针地址</param>
    /// <remarks>触发一个信号后，QWorkers会触发所有已注册的信号关联处理过程的执行</remarks>
    procedure Signal(AId: Integer; AData: Pointer = nil;
      AFreeType: TQJobDataFreeType = jdfFreeByUser); overload;
    /// <summary>按名称触发一个信号</summary>
    /// <param name="AName">信号名称</param>
    /// <param name="AData">附加给作业的用户数据指针地址</param>
    /// <remarks>触发一个信号后，QWorkers会触发所有已注册的信号关联处理过程的执行</remarks>
    procedure Signal(const AName: QStringW; AData: Pointer = nil;
      AFreeType: TQJobDataFreeType = jdfFreeByUser); overload;
    /// <summary>注册一个信号</summary>
    /// <param name="AName">信号名称</param>
    /// <remarks>
    /// 1.重复注册同一名称的信号将返回同一个编码
    /// 2.信号一旦注册，则只有程序退出时才会自动释放
    /// </remarks>
    function RegisterSignal(const AName: QStringW): Integer; // 注册一个信号名称

    procedure EnableWorkers;
    procedure DisableWorkers;
    /// <summary>最大允许工作者数量，不能小于2</summary>
    property MaxWorkers: Integer read FMaxWorkers write SetMaxWorkers;
    /// <summary>最小工作者数量，不能小于2<summary>
    property MinWorkers: Integer read FMinWorkers write SetMinWorkers;
    /// <summary>最大允许的长时间作业工作者数量，等价于允许开始的长时间作业数量</summary>
    property MaxLongtimeWorkers: Integer read FMaxLongtimeWorkers
      write SetMaxLongtimeWorkers;
    /// <summary>是否允许开始作业，如果为false，则投寄的作业都不会被执行，直到恢复为True</summary>
    /// <remarks>Enabled为False时已经运行的作业将仍然运行，它只影响尚未执行的作来</remarks>
    property Enabled: Boolean read GetEnabled write SetEnabled;
    /// <summary>是否正在释放TQWorkers对象自身</summary>
    property Terminating: Boolean read FTerminating;
  end;
{$IFDEF UNICODE}

  TQJobItemList = TList<PQJob>;
{$ELSE}
  TQJobItemList = TList;
{$ENDIF}

  TQJobGroup = class
  protected
    FEvent: TEvent; // 事件，用于等待作业完成
    FCount: Integer; // 组内的作业数量
    FLocker: TQSimpleLock;
    FItems: TQJobItemList; // 作业列表
    FPrepareCount: Integer; // 准备计数
    FByOrder: Boolean; // 是否按顺序触发作业，即必需等待上一个作业完成后才执行下一个
    FAfterDone: TNotifyEvent; // 作业完成事件通知
    FWaitResult: TWaitResult;
    FTimeout: Int64;
    FTag: Pointer;
    procedure DoJobExecuted(AJob: PQJob);
    procedure DoJobsTimeout(AJob: PQJob);
    procedure DoAfterDone;
  public
    /// AByOrder指定是否是顺序作业，作业之间必需依次执行
    constructor Create(AByOrder: Boolean = False); overload;
    destructor Destroy; override;
    procedure Cancel;
    // 准备添加作业，实际增加内部计数器
    procedure Prepare;
    // 减少内部计数器，如果计数器减为0，则开始实际执行作业
    procedure Run(ATimeout: Cardinal = INFINITE);
    // 添加一个作业过程，如果准备内部计数器为0，则直接执行，否则只添加到列表
    function Add(AProc: TQJobProc; AData: Pointer;
      AInMainThread: Boolean = False;
      AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean;
    // 等待作业完成，ATimeout为最长等待时间
    function WaitFor(ATimeout: Cardinal = INFINITE): TWaitResult;
    // 等待作业完成，ATimeout为最长等待时间，不同的是MsgWaitFor不阻塞消息处理
    function MsgWaitFor(ATimeout: Cardinal = INFINITE): TWaitResult;
    // 未完成的作业数量
    property Count: Integer read FCount;
    /// 全部作业执行完成时触发的回调事件
    property AfterDone: TNotifyEvent read FAfterDone write FAfterDone;
    /// 是否是按顺序执行，只能在构造函数中指定，此处只读
    property ByOrder: Boolean read FByOrder;
    property Tag: Pointer read FTag write FTag;
  end;

  /// <summary>将全局的作业处理函数转换为TQJobProc类型，以便正常调度使用</summary>
  /// <param name="AProc">全局的作业处理函数</param>
  /// <returns>返回新的TQJobProc实例</returns>
function MakeJobProc(const AProc: TQJobProcG): TQJobProc; overload;
// 获取系统中CPU的核心数量
function GetCPUCount: Integer;
// 获取当前系统的时间戳，最高可精确到0.1ms，但实际受操作系统限制
function GetTimestamp: Int64;
// 设置线程运行的CPU
procedure SetThreadCPU(AHandle: THandle; ACpuNo: Integer);
// 原子锁定与运算
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer;
// 原子锁定或运算
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer;
function JobPoolCount: NativeInt;
function JobPoolPrint: QStringW;

var
  Workers: TQWorkers;

implementation

resourcestring
  SNotSupportNow = '当前尚未支持功能 %s';
  STooFewWorkers = '指定的最小工作者数量太少(必需大于等于1)。';
  STooManyLongtimeWorker = '不能允许太多长时间作业线程(最多允许工作者一半)。';
  SBadWaitDoneParam = '未知的等待正在执行作业完成方式:%d';

type
{$IFDEF MSWINDOWS}
  TGetTickCount64 = function: Int64;
{$ENDIF MSWINDOWS}

  TJobPool = class
  protected
    FFirst: PQJob;
    FCount: Integer;
    FSize: Integer;
    FLocker: TQSimpleLock;
  public
    constructor Create(AMaxSize: Integer); overload;
    destructor Destroy; override;
    procedure Push(AJob: PQJob);
    function Pop: PQJob;
    property Count: Integer read FCount;
    property Size: Integer read FSize write FSize;
  end;

var
  JobPool: TJobPool;
{$IFDEF NEXTGEN}
  _Watch: TStopWatch;
{$ELSE}
  GetTickCount64: TGetTickCount64;
  _PerfFreq: Int64;
{$ENDIF}

procedure JobInitialize(AJob: PQJob; AData: Pointer;
  AFreeType: TQJobDataFreeType; ARunOnce, ARunInMainThread: Boolean); inline;
begin
AJob.Data := AData;
if AData <> nil then
  begin
  case AFreeType of
    jdfFreeAsObject:
      AJob.IsObjectOwner := true;
    jdfFreeAsRecord:
      AJob.IsRecordOwner := true;
    jdfFreeAsInterface:
      begin
      AJob.IsInterfaceOwner := true;
      IUnknown(AData)._AddRef;
      end;
  end;
  end;
AJob.SetFlags(JOB_RUN_ONCE, ARunOnce);
AJob.SetFlags(JOB_IN_MAINTHREAD, ARunInMainThread);
end;

// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
repeat
  Result := Dest;
  i := Result and AMask;
until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
repeat
  Result := Dest;
  i := Result or AMask;
until AtomicCmpExchange(Dest, i, Result) = Result;
end;
{$IFDEF MSWINDOWS}
// function InterlockedCompareExchange64
{$ENDIF}

procedure SetThreadCPU(AHandle: THandle; ACpuNo: Integer);
begin
{$IFDEF MSWINDOWS}
SetThreadIdealProcessor(AHandle, ACpuNo);
{$ELSE}
// Linux/Andriod/iOS暂时忽略,XE6未引入sched_setaffinity定义
{$ENDIF}
end;

// 返回值的时间精度为100ns，即0.1ms
function GetTimestamp: Int64;
begin
{$IFDEF NEXTGEN}
Result := _Watch.Elapsed.Ticks div 1000;
{$ELSE}
if _PerfFreq > 0 then
  begin
  QueryPerformanceCounter(Result);
  Result := Result * 10000 div _PerfFreq;
  end
else if Assigned(GetTickCount64) then
  Result := GetTickCount64 * 10000
else
  Result := GetTickCount * 10000;
{$ENDIF}
end;

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
GetSystemInfo(si);
Result := si.dwNumberOfProcessors;
{$ELSE}// Linux,MacOS,iOS,Andriod{POSIX}
{$IFDEF POSIX}
Result := sysconf(_SC_NPROCESSORS_ONLN);
{$ELSE}// 不认识的操作系统，CPU数默认为1
Result := 1;
{$ENDIF !POSIX}
{$ENDIF !MSWINDOWS}
end;

function MakeJobProc(const AProc: TQJobProcG): TQJobProc;
begin
TMethod(Result).Data := nil;
TMethod(Result).Code := @AProc;
end;

function SameWorkerProc(const P1, P2: TQJobProc): Boolean; inline;
begin
Result := (TMethod(P1).Code = TMethod(P2).Code) and
  (TMethod(P1).Data = TMethod(P2).Data);
end;
{ TQJob }

procedure TQJob.AfterRun(AUsedTime: Int64);
begin
Inc(Runs);
if AUsedTime > 0 then
  begin
  Inc(TotalUsedTime, AUsedTime);
  if MinUsedTime = 0 then
    MinUsedTime := AUsedTime
  else if MinUsedTime > AUsedTime then
    MinUsedTime := AUsedTime;
  if MaxUsedTime = 0 then
    MaxUsedTime := AUsedTime
  else if MaxUsedTime < AUsedTime then
    MaxUsedTime := AUsedTime;
  end;
end;

procedure TQJob.Assign(const ASource: PQJob);
begin
Self := ASource^;
// StartTime := ASource.StartTime;
// PushTime := ASource.PushTime; // 入队时间
// PopTime := ASource.PopTime;
// NextTime := ASource.NextTime;
// WorkerProc := ASource.WorkerProc; // 作业处理函数+8/16B
// {$IFDEF UNICODE}
// WorkerProcA := ASource.WorkerProcA;
// {$ENDIF}
// Runs := ASource.Runs;
// MinUsedTime := ASource.MinUsedTime; // 最小运行时间+4B
// TotalUsedTime := ASource.TotalUsedTime;
// MaxUsedTime := ASource.MaxUsedTime;
// Flags := ASource.Flags;
// Data := ASource.Data;
// SignalId := ASource.SignalId;
// Runs:=AJob.Runs;
// 下面三个成员不拷贝
Worker := nil;
Next := nil;
Source := nil;
end;

constructor TQJob.Create(AProc: TQJobProc);
begin
WorkerProc := AProc;
SetFlags(JOB_RUN_ONCE, true);
end;

function TQJob.GetAvgTime: Integer;
begin
if Runs > 0 then
  Result := TotalUsedTime div Runs
else
  Result := 0;
end;

function TQJob.GetIsTerminated: Boolean;
begin
if Assigned(Worker) then
  Result := Workers.Terminating or Worker.Terminated or
    ((Flags and JOB_TERMINATED) <> 0) or (Worker.FTerminatingJob = @Self)
else
  Result := (Flags and JOB_TERMINATED) <> 0;
end;

function TQJob.GetEscapedTime: Int64;
begin
Result := GetTimestamp - StartTime;
end;

function TQJob.GetFlags(AIndex: Integer): Boolean;
begin
Result := (Flags and AIndex) <> 0;
end;

procedure TQJob.Reset;
begin
FillChar(Self, SizeOf(TQJob), 0);
end;

procedure TQJob.SetFlags(AIndex: Integer; AValue: Boolean);
begin
if AValue then
  Flags := (Flags or AIndex)
else
  Flags := (Flags and (not AIndex));
end;

procedure TQJob.SetIsTerminated(const Value: Boolean);
begin
SetFlags(JOB_TERMINATED, Value);
end;

procedure TQJob.Synchronize(AMethod: TThreadMethod);
begin
Worker.Synchronize(AMethod);
end;

procedure TQJob.UpdateNextTime;
begin
if (Runs = 0) and (FirstDelay <> 0) then
  NextTime := PushTime + FirstDelay
else if Interval <> 0 then
  begin
  if NextTime = 0 then
    NextTime := GetTimestamp + Interval
  else
    Inc(NextTime, Interval);
  end
else
  NextTime := GetTimestamp;
end;

{ TQSimpleJobs }

function TQSimpleJobs.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
var
  AFirst, AJob, APrior, ANext: PQJob;
  ACount: Integer;
begin
// 先将SimpleJobs所有的异步作业清空，以防止被弹出执行
FLocker.Enter;
AJob := FFirst;
ACount := FCount;
FFirst := nil;
FLast := nil;
FCount := 0;
FLocker.Leave;

Result := 0;
APrior := nil;
AFirst := nil;
while (AJob <> nil) and (AMaxTimes <> 0) do
  begin
  ANext := AJob.Next;
  if TMethod(AJob.WorkerProc).Data = AObject then
    begin
    if APrior <> nil then
      APrior.Next := ANext;
    AJob.Next := nil;
    FOwner.FreeJob(AJob);
    Dec(AMaxTimes);
    Inc(Result);
    Dec(ACount);
    end
  else
    begin
    if AFirst = nil then
      AFirst := AJob;
    APrior := AJob;
    end;
  AJob := ANext;
  end;
if ACount > 0 then
  begin
  FLocker.Enter;
  AFirst.Next := FFirst;
  FFirst := AFirst;
  Inc(FCount, ACount);
  if FLast = nil then
    FLast := APrior;
  FLocker.Leave;
  end;
end;

function TQSimpleJobs.Clear(AProc: TQJobProc; AData: Pointer;
  AMaxTimes: Integer): Integer;
var
  AFirst, AJob, APrior, ANext: PQJob;
  ACount: Integer;
begin
FLocker.Enter;
AJob := FFirst;
ACount := FCount;
FFirst := nil;
FLast := nil;
FCount := 0;
FLocker.Leave;
Result := 0;
APrior := nil;
AFirst := nil;
while (AJob <> nil) and (AMaxTimes <> 0) do
  begin
  ANext := AJob.Next;
  if SameWorkerProc(AJob.WorkerProc, AProc) and (AJob.Data = AData) then
    begin
    if APrior <> nil then
      APrior.Next := ANext;
    FOwner.FreeJob(AJob);
    Dec(AMaxTimes);
    Inc(Result);
    Dec(ACount);
    end
  else
    begin
    if AFirst = nil then
      AFirst := AJob;
    APrior := AJob;
    end;
  AJob := ANext;
  end;
if ACount > 0 then
  begin
  FLocker.Enter;
  AFirst.Next := FFirst;
  FFirst := AFirst;
  Inc(FCount, ACount);
  if FLast = nil then
    FLast := APrior;
  FLocker.Leave;
  end;
end;

procedure TQSimpleJobs.Clear;
var
  AFirst: PQJob;
begin
FLocker.Enter;
AFirst := FFirst;
FFirst := nil;
FLast := nil;
FCount := 0;
FLocker.Leave;
FOwner.FreeJob(AFirst);
end;

constructor TQSimpleJobs.Create(AOwner: TQWorkers);
begin
inherited Create(AOwner);
FLocker := TQSimpleLock.Create;
end;

destructor TQSimpleJobs.Destroy;
begin
inherited;
FreeObject(FLocker);
end;

function TQSimpleJobs.GetCount: Integer;
begin
Result := FCount;
end;

function TQSimpleJobs.InternalPop: PQJob;
begin
FLocker.Enter;
Result := FFirst;
if Result <> nil then
  begin
  FFirst := Result.Next;
  if FFirst = nil then
    FLast := nil;
  Dec(FCount);
  end;
FLocker.Leave;
end;

function TQSimpleJobs.InternalPush(AJob: PQJob): Boolean;
begin
FLocker.Enter;
if FLast = nil then
  FFirst := AJob
else
  FLast.Next := AJob;
FLast := AJob;
Inc(FCount);
FLocker.Leave;
Result := true;
end;

{ TQJobs }

procedure TQJobs.Clear;
var
  AItem: PQJob;
begin
repeat
  AItem := Pop;
  if AItem <> nil then
    FOwner.FreeJob(AItem)
  else
    Break;
until 1 > 2;
end;

constructor TQJobs.Create(AOwner: TQWorkers);
begin
inherited Create;
FOwner := AOwner;
end;

destructor TQJobs.Destroy;
begin
Clear;
inherited;
end;

function TQJobs.GetEmpty: Boolean;
begin
Result := (Count = 0);
end;

function TQJobs.Pop: PQJob;
begin
Result := InternalPop;
if Result <> nil then
  begin
  Result.PopTime := GetTimestamp;
  Result.Next := nil;
  end;
end;

function TQJobs.Push(AJob: PQJob): Boolean;
begin
AJob.Owner := Self;
AJob.PushTime := GetTimestamp;
Result := InternalPush(AJob);
if not Result then
  begin
  AJob.Next := nil;
  FOwner.FreeJob(AJob);
  end;
end;

{ TQRepeatJobs }

procedure TQRepeatJobs.Clear;
begin
FLocker.Enter;
try
  FItems.Clear;
finally
  FLocker.Leave;
end;
end;

function TQRepeatJobs.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
var
  ANode, ANext: TQRBNode;
  APriorJob, AJob, ANextJob: PQJob;
  ACanDelete: Boolean;
begin
// 现在清空重复的计划作业
Result := 0;
FLocker.Enter;
try
  ANode := FItems.First;
  while (ANode <> nil) and (AMaxTimes <> 0) do
    begin
    ANext := ANode.Next;
    AJob := ANode.Data;
    ACanDelete := true;
    APriorJob := nil;
    while AJob <> nil do
      begin
      ANextJob := AJob.Next;
      if TMethod(AJob.WorkerProc).Data = AObject then
        begin
        if ANode.Data = AJob then
          ANode.Data := AJob.Next;
        if Assigned(APriorJob) then
          APriorJob.Next := AJob.Next;
        AJob.Next := nil;
        FOwner.FreeJob(AJob);
        Dec(AMaxTimes);
        Inc(Result);
        end
      else
        begin
        ACanDelete := False;
        APriorJob := AJob;
        end;
      AJob := ANextJob;
      end;
    if ACanDelete then
      FItems.Delete(ANode);
    ANode := ANext;
    end;
  if FItems.Count > 0 then
    FFirstFireTime := PQJob(FItems.First.Data).NextTime
  else
    FFirstFireTime := 0;
finally
  FLocker.Leave;
end;
end;

procedure TQRepeatJobs.AfterJobRun(AJob: PQJob; AUsedTime: Int64);
var
  ANode: TQRBNode;
  function UpdateSource: Boolean;
  var
    ATemp, APrior: PQJob;
  begin
  Result := False;
  ATemp := ANode.Data;
  APrior := nil;
  while ATemp <> nil do
    begin
    if ATemp = AJob.Source then
      begin
      if AJob.IsTerminated then
        begin
        if APrior <> nil then
          APrior.Next := ATemp.Next
        else
          ANode.Data := ATemp.Next;
        ATemp.Next := nil;
        FOwner.FreeJob(ATemp);
        if ANode.Data = nil then
          FItems.Delete(ANode);
        end
      else
        ATemp.AfterRun(AUsedTime);
      Result := true;
      Break;
      end;
    APrior := ATemp;
    ATemp := ATemp.Next;
    end;
  end;

begin
FLocker.Enter;
try
  ANode := FItems.Find(AJob);
  if ANode <> nil then
    begin
    if UpdateSource then
      Exit;
    end;
  ANode := FItems.First;
  while ANode <> nil do
    begin
    if UpdateSource then
      Break;
    ANode := ANode.Next;
    end;
finally
  FLocker.Leave;
end;
end;

function TQRepeatJobs.Clear(AProc: TQJobProc; AData: Pointer;
  AMaxTimes: Integer): Integer;
var
  AJob, APrior, ANext: PQJob;
  ANode, ANextNode: TQRBNode;
begin
Result := 0;
FLocker.Enter;
try
  ANode := FItems.First;
  while (ANode <> nil) and (AMaxTimes <> 0) do
    begin
    AJob := ANode.Data;
    APrior := nil;
    repeat
      if SameWorkerProc(AJob.WorkerProc, AProc) and
        ((AData = Pointer(-1)) or (AData = AJob.Data)) then
        begin
        ANext := AJob.Next;
        if APrior = nil then
          ANode.Data := ANext
        else
          APrior.Next := AJob.Next;
        FOwner.FreeJob(AJob);
        AJob := ANext;
        Dec(AMaxTimes);
        Inc(Result);
        end
      else
        begin
        APrior := AJob;
        AJob := AJob.Next
        end;
    until AJob = nil;
    if ANode.Data = nil then
      begin
      ANextNode := ANode.Next;
      FItems.Delete(ANode);
      ANode := ANextNode;
      end
    else
      ANode := ANode.Next;
    end;
  if FItems.Count > 0 then
    FFirstFireTime := PQJob(FItems.First.Data).NextTime
  else
    FFirstFireTime := 0;
finally
  FLocker.Leave;
end;
end;

constructor TQRepeatJobs.Create(AOwner: TQWorkers);
begin
inherited;
FItems := TQRBTree.Create(DoTimeCompare);
FItems.OnDelete := DoJobDelete;
FLocker := TCriticalSection.Create;
end;

destructor TQRepeatJobs.Destroy;
begin
inherited;
FreeObject(FItems);
FreeObject(FLocker);
end;

procedure TQRepeatJobs.DoJobDelete(ATree: TQRBTree; ANode: TQRBNode);
begin
FOwner.FreeJob(ANode.Data);
end;

function TQRepeatJobs.DoTimeCompare(P1, P2: Pointer): Integer;
begin
Result := PQJob(P1).NextTime - PQJob(P2).NextTime;
end;

function TQRepeatJobs.GetCount: Integer;
begin
Result := FItems.Count;
end;

function TQRepeatJobs.InternalPop: PQJob;
var
  ANode: TQRBNode;
  ATick: Int64;
  AJob: PQJob;
begin
Result := nil;
if FItems.Count = 0 then
  Exit;
FLocker.Enter;
try
  if FItems.Count > 0 then
    begin
    ATick := GetTimestamp;
    ANode := FItems.First;
    if PQJob(ANode.Data).NextTime <= ATick then
      begin
      AJob := ANode.Data;
      // OutputDebugString(PWideChar('Result.NextTime='+IntToStr(Result.NextTime)+',Current='+IntToStr(ATick)));
      if AJob.Next <> nil then // 如果没有更多需要执行的作业，则删除结点，否则指向下一个
        ANode.Data := AJob.Next
      else
        begin
        ANode.Data := nil;
        FItems.Delete(ANode);
        ANode := FItems.First;
        if ANode <> nil then
          FFirstFireTime := PQJob(ANode.Data).NextTime
        else // 没有计划作业了，不需要了
          FFirstFireTime := 0;
        end;
      if AJob.Runonce then
        Result := AJob
      else
        begin
        Inc(AJob.NextTime, AJob.Interval);
        Result := JobPool.Pop;
        Result.Assign(AJob);
        Result.Source := AJob;
        // 重新插入作业
        ANode := FItems.Find(AJob);
        if ANode = nil then
          begin
          FItems.Insert(AJob);
          FFirstFireTime := PQJob(FItems.First.Data).NextTime;
          end
        else // 如果已经存在同一时刻的作业，则自己挂接到其它作业头部
          begin
          AJob.Next := PQJob(ANode.Data);
          ANode.Data := AJob; // 首个作业改为自己
          end;
        end;
      end;
    end;
finally
  FLocker.Leave;
end;
end;

function TQRepeatJobs.InternalPush(AJob: PQJob): Boolean;
var
  ANode: TQRBNode;
begin
// 计算作业的下次执行时间
AJob.UpdateNextTime;
FLocker.Enter;
try
  ANode := FItems.Find(AJob);
  if ANode = nil then
    begin
    FItems.Insert(AJob);
    FFirstFireTime := PQJob(FItems.First.Data).NextTime;
    end
  else // 如果已经存在同一时刻的作业，则自己挂接到其它作业头部
    begin
    AJob.Next := PQJob(ANode.Data);
    ANode.Data := AJob; // 首个作业改为自己
    end;
  Result := true;
finally
  FLocker.Leave;
end;
end;

{ TQWorker }

procedure TQWorker.ComNeeded(AInitFlags: Cardinal);
begin
{$IFDEF MSWINDOWS}
if not ComInitialized then
  begin
  if AInitFlags = 0 then
    CoInitialize(nil)
  else
    CoInitializeEx(nil, AInitFlags);
  FFlags := FFlags or WORKER_COM_INITED;
  end;
{$ENDIF MSWINDOWS}
end;

constructor TQWorker.Create(AOwner: TQWorkers);
begin
inherited Create(true);
FOwner := AOwner;
FTimeout := 1000;
FreeOnTerminate := true;
FFlags := WORKER_ISBUSY; // 默认为忙碌
AtomicIncrement(AOwner.FBusyCount);
FEvent := TEvent.Create(nil, False, False, '');
end;

destructor TQWorker.Destroy;
begin
FreeObject(FEvent);
inherited;
end;

procedure TQWorker.DoJob(AJob: PQJob);
begin
{$IFDEF UNICODE}
if AJob.IsAnonWorkerProc then
  AJob.WorkerProcA(AJob)
else
{$ENDIF}
  AJob.WorkerProc(AJob);
end;

procedure TQWorker.Execute;
var
  wr: TWaitResult;
{$IFDEF MSWINDOWS}
  SyncEvent: TEvent;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
SyncEvent := TEvent.Create(nil, False, False, '');
{$ENDIF}
try
  while not(Terminated or FOwner.FTerminating) do
    begin
    if FOwner.Enabled and (FOwner.FRepeatJobs.FFirstFireTime <> 0) then
      begin
      FTimeout := (FOwner.FRepeatJobs.FFirstFireTime - GetTimestamp) div 10;
      if FTimeout < 0 then // 时间已经到了？那么立刻执行
        FTimeout := 0;
      end
    else
      FTimeout := 15000; // 15S如果仍没有作业进入，则除非自己是保留的线程对象，否则释放工作者
    if FTimeout <> 0 then
      begin
      wr := FEvent.WaitFor(FTimeout);
      if Terminated or FOwner.FTerminating then
        Break;
      end
    else
      wr := wrSignaled;
    if (wr = wrSignaled) or ((FOwner.FRepeatJobs.FFirstFireTime <> 0) and
      (FOwner.FRepeatJobs.FFirstFireTime + 10 >= GetTimestamp)) then
      begin
      if FOwner.FTerminating then
        Break;
      if IsIdle then
        begin
        SetFlags(WORKER_ISBUSY or WORKER_LOOKUP, true);
        FOwner.WorkerBusy(Self);
        end
      else
        SetFlags(WORKER_LOOKUP, true);
      repeat
        FActiveJob := FOwner.Popup;
        if FActiveJob <> nil then
          begin
          FActiveJob.Worker := Self;
          FActiveJobProc := FActiveJob.WorkerProc;
          // 为Clear(AObject)准备判断，以避免FActiveJob线程不安全
          FActiveJobData := FActiveJob.Data;
          if FActiveJob.IsSignalWakeup then
            FActiveJobSource := FActiveJob.Source
          else
            FActiveJobSource := nil;
          if FActiveJob.IsGrouped then
            FActiveJobGroup := FActiveJob.Group
          else
            FActiveJobGroup := nil;
          FActiveJobFlags := FActiveJob.Flags;
          if FActiveJob.StartTime = 0 then
            begin
            FActiveJob.StartTime := GetTimestamp;
            FActiveJob.FirstStartTime := FActiveJob.StartTime;
            end
          else
            FActiveJob.StartTime := GetTimestamp;
          try
            FFlags := (FFlags or WORKER_EXECUTING) and (not WORKER_LOOKUP);
            if FActiveJob.InMainThread then
{$IFDEF MSWINDOWS}
              begin
              if PostMessage(FOwner.FMainWorker, WM_APP, WPARAM(FActiveJob),
                LPARAM(SyncEvent)) then
                SyncEvent.WaitFor(INFINITE);
              end
{$ELSE}
              Synchronize(Self, FireInMainThread)
{$ENDIF}
            else
              DoJob(FActiveJob);
          except
          end;
          if not FActiveJob.Runonce then
            FOwner.FRepeatJobs.AfterJobRun(FActiveJob,
              GetTimestamp - FActiveJob.StartTime)
          else
            begin
            if FActiveJob.IsSignalWakeup then
              FOwner.SignalWorkDone(FActiveJob,
                GetTimestamp - FActiveJob.StartTime)
            else if FActiveJob.IsLongtimeJob then
              AtomicDecrement(FOwner.FLongTimeWorkers)
            else if FActiveJob.IsGrouped then
              FActiveJobGroup.DoJobExecuted(FActiveJob);
            FActiveJob.Worker := nil;
            end;
          FOwner.FreeJob(FActiveJob);
          FActiveJobProc := nil;
          FActiveJobSource := nil;
          FActiveJobFlags := 0;
          FActiveJobGroup := nil;
          FTerminatingJob := nil;
          FFlags := FFlags and (not WORKER_EXECUTING);
          end
        else
          FFlags := FFlags and (not WORKER_LOOKUP);
      until (FActiveJob = nil) or FOwner.FTerminating or Terminated or
        (not FOwner.Enabled);
      SetFlags(WORKER_ISBUSY, False);
      FOwner.WorkerIdle(Self, irNoJob);
      end
    else if (not IsReserved) and (FTimeout = 15000) then
      begin
      SetFlags(WORKER_ISBUSY, False);
      FOwner.WorkerIdle(Self, irTimeout);
      end;
    end;
finally
  FOwner.WorkerTerminate(Self);
{$IFDEF MSWINDOWS}
  if ComInitialized then
    CoUninitialize;
  FreeObject(SyncEvent);
{$ENDIF}
end;
end;

procedure TQWorker.FireInMainThread;
begin
DoJob(FActiveJob);
end;

function TQWorker.GetFlags(AIndex: Integer): Boolean;
begin
Result := ((FFlags and AIndex) <> 0);
end;

function TQWorker.GetIsIdle: Boolean;
begin
Result := not IsBusy;
end;

procedure TQWorker.SetFlags(AIndex: Integer; AValue: Boolean);
begin
if AValue then
  FFlags := FFlags or AIndex
else
  FFlags := FFlags and (not AIndex);
end;

{ TQWorkers }

function TQWorkers.Post(AJob: PQJob): Boolean;
begin
if (not FTerminating) and (Assigned(AJob.WorkerProc)
{$IFDEF UNICODE} or Assigned(AJob.WorkerProcA){$ENDIF}) then
  begin
  if AJob.Runonce and (AJob.FirstDelay = 0) then
    Result := FSimpleJobs.Push(AJob)
  else
    Result := FRepeatJobs.Push(AJob);
  if Result then
    LookupIdleWorker;
  end
else
  begin
  AJob.Next := nil;
  FreeJob(AJob);
  Result := False;
  end;
end;

function TQWorkers.Post(AProc: TQJobProc; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, true, ARunInMainThread);
AJob.WorkerProc := AProc;
Result := Post(AJob);
end;

function TQWorkers.Post(AProc: TQJobProc; AInterval: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.Interval := AInterval;
AJob.WorkerProc := AProc;
Result := Post(AJob);
end;

function TQWorkers.Post(AProc: TQJobProcG; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
begin
Result := Post(MakeJobProc(AProc), AData, ARunInMainThread, AFreeType);
end;

{$IFDEF UNICODE}

function TQWorkers.Post(AProc: TQJobProcA; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, true, ARunInMainThread);
AJob.WorkerProcA := AProc;
AJob.IsAnonWorkerProc := true;
Result := Post(AJob);
end;
{$ENDIF}

function TQWorkers.Clear(AObject: Pointer; AMaxTimes: Integer): Integer;
var
  ACleared: Integer;
  AWaitParam: TWorkerWaitParam;
  function ClearSignalJobs: Integer;
  var
    i: Integer;
    AJob, ANext, APrior: PQJob;
    AList: PQHashList;
    ASignal: PQSignal;
  begin
  Result := 0;
  FLocker.Enter;
  try
    for i := 0 to FSignalJobs.BucketCount - 1 do
      begin
      AList := FSignalJobs.Buckets[i];
      if AList <> nil then
        begin
        ASignal := AList.Data;
        if ASignal.First <> nil then
          begin
          AJob := ASignal.First;
          APrior := nil;
          while (AJob <> nil) and (AMaxTimes <> 0) do
            begin
            ANext := AJob.Next;
            if TMethod(AJob.WorkerProc).Data = AObject then
              begin
              if ASignal.First = AJob then
                ASignal.First := ANext;
              if Assigned(APrior) then
                APrior.Next := ANext;
              AJob.Next := nil;
              FreeJob(AJob);
              Dec(AMaxTimes);
              Inc(Result);
              end
            else
              APrior := AJob;
            AJob := ANext;
            end;
          if AMaxTimes = 0 then
            Break;
          end;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  end;

begin
Result := 0;
if Self <> nil then
  begin
  ACleared := FSimpleJobs.Clear(AObject, AMaxTimes);
  Inc(Result, ACleared);
  Dec(AMaxTimes, ACleared);
  if AMaxTimes <> 0 then
    begin
    ACleared := FRepeatJobs.Clear(AObject, AMaxTimes);
    Inc(Result, ACleared);
    if AMaxTimes <> 0 then
      begin
      ACleared := ClearSignalJobs;
      Inc(Result, ACleared);
      if AMaxTimes <> 0 then
        begin
        AWaitParam.WaitType := 0;
        AWaitParam.Bound := AObject;
        WaitRunningDone(AWaitParam);
        end;
      end;
    end;
  end;
end;

function TQWorkers.At(AProc: TQJobProc; const ADelay, AInterval: Int64;
  AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.WorkerProc := AProc;
AJob.Interval := AInterval;
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;

function TQWorkers.At(AProc: TQJobProc; const ATime: TDateTime;
  const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
  ADelay: Int64;
  ANow, ATemp: TDateTime;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.WorkerProc := AProc;
AJob.Interval := AInterval;
// ATime我们只要时间部分，日期忽略
ANow := Now;
ANow := ANow - Trunc(ANow);
ATemp := ATime - Trunc(ATime);
if ANow > ATemp then // 好吧，今天的点已经过了，算明天
  ADelay := Trunc(((1 + ANow) - ATemp) * Q1Day) // 延迟的时间，单位为0.1ms
else
  ADelay := Trunc((ATemp - ANow) * Q1Day);
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;
{$IFDEF UNICODE}

function TQWorkers.At(AProc: TQJobProcA; const ATime: TDateTime;
  const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
  ADelay: Int64;
  ANow, ATemp: TDateTime;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.WorkerProcA := AProc;
AJob.IsAnonWorkerProc := true;
AJob.Interval := AInterval;
// ATime我们只要时间部分，日期忽略
ANow := Now;
ANow := ANow - Trunc(ANow);
ATemp := ATime - Trunc(ATime);
if ANow > ATemp then // 好吧，今天的点已经过了，算明天
  ADelay := Trunc(((1 + ANow) - ATemp) * Q1Day) // 延迟的时间，单位为0.1ms
else
  ADelay := Trunc((ATemp - ANow) * Q1Day);
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;
{$ENDIF}

function TQWorkers.Clear(AProc: TQJobProc; AData: Pointer;
  AMaxTimes: Integer): Integer;
var
  ACleared: Integer;
  AWaitParam: TWorkerWaitParam;
  function ClearSignalJobs: Integer;
  var
    i: Integer;
    AJob, ANext, APrior: PQJob;
    AList: PQHashList;
    ASignal: PQSignal;
  begin
  Result := 0;
  FLocker.Enter;
  try
    for i := 0 to FSignalJobs.BucketCount - 1 do
      begin
      AList := FSignalJobs.Buckets[i];
      if AList <> nil then
        begin
        ASignal := AList.Data;
        if ASignal.First <> nil then
          begin
          AJob := ASignal.First;
          APrior := nil;
          while (AJob <> nil) and (AMaxTimes <> 0) do
            begin
            ANext := AJob.Next;
            if SameWorkerProc(AJob.WorkerProc, AProc) and
              ((AData = Pointer(-1)) or (AJob.Data = AData)) then
              begin
              if ASignal.First = AJob then
                ASignal.First := ANext;
              if Assigned(APrior) then
                APrior.Next := ANext;
              AJob.Next := nil;
              FreeJob(AJob);
              Inc(Result);
              Dec(AMaxTimes);
              end
            else
              APrior := AJob;
            AJob := ANext;
            end;
          if AMaxTimes = 0 then
            Break;
          end;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  end;

begin
Result := 0;
if Self <> nil then
  begin
  ACleared := FSimpleJobs.Clear(AProc, AData, AMaxTimes);
  Dec(AMaxTimes, ACleared);
  Inc(Result, ACleared);
  if AMaxTimes <> 0 then
    begin
    ACleared := FRepeatJobs.Clear(AProc, AData, AMaxTimes);
    Dec(AMaxTimes, ACleared);
    Inc(Result, ACleared);
    if AMaxTimes <> 0 then
      begin
      ACleared := ClearSignalJobs;
      Inc(Result, ACleared);
      if AMaxTimes <> 0 then
        begin
        AWaitParam.WaitType := 1;
        AWaitParam.Data := AData;
        AWaitParam.WorkerProc := TMethod(AProc);
        WaitRunningDone(AWaitParam);
        end;
      end;
    end;
  end;
end;

procedure TQWorkers.ClearWorkers;
var
  i: Integer;
{$IFDEF MSWINDOWS}
  function WorkerExists: Boolean;
  var
    J: Integer;
    ACode: Cardinal;
  begin
  Result := False;
  FLocker.Enter;
  try
    while FWorkerCount > 0 do
      begin
      if GetExitCodeThread(FWorkers[0].Handle, ACode) then
        begin
        if ACode = STILL_ACTIVE then
          begin
          Result := true;
          Break;
          end;
        end;
      // 工作者已经不存在，可能被外部线程结束
      FreeObject(FWorkers[0]);
      if FWorkerCount > 0 then
        begin
        for J := 1 to FWorkerCount - 1 do
          FWorkers[J - 1] := FWorkers[J];
        Dec(FWorkerCount);
        FWorkers[FWorkerCount] := nil;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  end;
{$ENDIF}

begin
FTerminating := true;
FLocker.Enter;
try
  FRepeatJobs.FFirstFireTime := 0;
  for i := 0 to FWorkerCount - 1 do
    FWorkers[i].FEvent.SetEvent;
finally
  FLocker.Leave;
end;
while (FWorkerCount > 0) {$IFDEF MSWINDOWS} and WorkerExists {$ENDIF} do
{$IFDEF MSWINDOWS}
  SwitchToThread;
{$ELSE}
  TThread.Yield;
{$ENDIF}
end;

constructor TQWorkers.Create(AMinWorkers: Integer);
var
  ACpuCount: Integer;
  i: Integer;
begin
FSimpleJobs := TQSimpleJobs.Create(Self);
FRepeatJobs := TQRepeatJobs.Create(Self);
FSignalJobs := TQHashTable.Create();
FSignalJobs.OnDelete := DoJobFree;
FSignalJobs.AutoSize := true;
ACpuCount := GetCPUCount;
if AMinWorkers < 1 then
  FMinWorkers := 2
else
  FMinWorkers := AMinWorkers; // 最少工作者为2个
FMaxWorkers := (ACpuCount shl 1) + 1;
if FMaxWorkers <= FMinWorkers then
  FMaxWorkers := (FMinWorkers shl 1) + 1;
FLocker := TCriticalSection.Create;
FTerminating := False;
// 创建默认工作者
FWorkerCount := FMinWorkers;
SetLength(FWorkers, FMaxWorkers);
for i := 0 to FMinWorkers - 1 do
  begin
  FWorkers[i] := TQWorker.Create(Self);
  FWorkers[i].SetFlags(WORKER_RESERVED, true); // 保留，不需要空闲检查
  FWorkers[i].Suspended := False;
  SetThreadCPU(FWorkers[i].Handle, i mod ACpuCount);
  end;
FMaxLongtimeWorkers := (FMaxWorkers shr 1);
{$IFDEF MSWINDOWS}
FMainWorker := AllocateHWnd(DoMainThreadWork);
{$ENDIF}
end;

function TQWorkers.Delay(AProc: TQJobProc; ADelay: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, true, ARunInMainThread);
AJob.WorkerProc := AProc;
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;
{$IFDEF UNICODE}

function TQWorkers.Delay(AProc: TQJobProcA; ADelay: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, true, ARunInMainThread);
AJob.WorkerProcA := AProc;
AJob.IsAnonWorkerProc := true;
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;
{$ENDIF}

function TQWorkers.Delay(AProc: TQJobProcG; ADelay: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
begin
Result := Delay(MakeJobProc(AProc), ADelay, AData, ARunInMainThread, AFreeType);
end;

destructor TQWorkers.Destroy;
begin
ClearWorkers;
FLocker.Enter;
try
  FreeObject(FSimpleJobs);
  FreeObject(FRepeatJobs);
  FreeObject(FSignalJobs);
finally
  FreeObject(FLocker);
end;
{$IFDEF MSWINDOWS}
DeallocateHWnd(FMainWorker);
{$ENDIF}
inherited;
end;

procedure TQWorkers.DisableWorkers;
begin
AtomicIncrement(FDisableCount);
end;

procedure TQWorkers.DoJobFree(ATable: TQHashTable; AHash: Cardinal;
  AData: Pointer);
var
  ASignal: PQSignal;
begin
ASignal := AData;
if ASignal.First <> nil then
  FreeJob(ASignal.First);
Dispose(ASignal);
end;
{$IFDEF MSWINDOWS}

procedure TQWorkers.DoMainThreadWork(var AMsg: TMessage);
var
  AJob: PQJob;
begin
if AMsg.Msg = WM_APP then
  begin
  try
    AJob := PQJob(AMsg.WPARAM);
    AJob.Worker.DoJob(AJob);
  finally
    if AMsg.LPARAM <> 0 then
      TEvent(AMsg.LPARAM).SetEvent;
  end;
  end
else
  AMsg.Result := DefWindowProc(FMainWorker, AMsg.Msg, AMsg.WPARAM, AMsg.LPARAM);
end;

{$ENDIF}

procedure TQWorkers.EnableWorkers;
var
  ANeedCount: Integer;
begin
if AtomicDecrement(FDisableCount) = 0 then
  begin
  if (FSimpleJobs.Count > 0) or (FRepeatJobs.Count > 0) then
    begin
    ANeedCount := FSimpleJobs.Count + FRepeatJobs.Count;
    while ANeedCount > 0 do
      begin
      if not LookupIdleWorker then
        Break;
      Dec(ANeedCount);
      end;
    end;
  end;
end;

procedure TQWorkers.FireSignalJob(ASignal: PQSignal; AData: Pointer;
  AFreeType: TQJobDataFreeType);
var
  AJob, ACopy: PQJob;
  ACount: PInteger;
begin
Inc(ASignal.Fired);
if AData <> nil then
  begin
  New(ACount);
  ACount^ := 1; // 初始值
  end
else
  ACount := nil;
AJob := ASignal.First;
while AJob <> nil do
  begin
  ACopy := JobPool.Pop;
  ACopy.Assign(AJob);
  JobInitialize(ACopy, AData, AFreeType, true, AJob.InMainThread);
  if ACount <> nil then
    begin
    AtomicIncrement(ACount^);
    ACopy.RefCount := ACount;
    end;
  ACopy.Source := AJob;
  FSimpleJobs.Push(ACopy);
  AJob := AJob.Next;
  end;
if AData <> nil then
  begin
  if AtomicDecrement(ACount^) = 0 then
    begin
    Dispose(ACount);
    FreeJobData(AData, AFreeType);
    end;
  end;
end;

procedure TQWorkers.FreeJob(AJob: PQJob);
var
  ANext: PQJob;
  AFreeData: Boolean;
begin
while AJob <> nil do
  begin
  ANext := AJob.Next;
  if AJob.Data <> nil then
    begin
    if AJob.IsSignalWakeup then
      begin
      AFreeData := AtomicDecrement(AJob.RefCount^) = 0;
      if AFreeData then
        Dispose(AJob.RefCount);
      end
    else
      AFreeData := AJob.IsDataOwner;
    if AFreeData then
      begin
      if AJob.IsObjectOwner then
        FreeJobData(AJob.Data, jdfFreeAsObject)
      else if AJob.IsRecordOwner then
        FreeJobData(AJob.Data, jdfFreeAsRecord)
      else if AJob.IsInterfaceOwner then
        FreeJobData(AJob.Data, jdfFreeAsInterface);
      end;
    end;
  JobPool.Push(AJob);
  AJob := ANext;
  end;
end;

procedure TQWorkers.FreeJobData(AData: Pointer; AFreeType: TQJobDataFreeType);
begin
case AFreeType of
  jdfFreeAsObject:
    try
      FreeObject(TObject(AData));
    except
    end;
  jdfFreeAsRecord:
    try
      Dispose(AData);
    except
    end;
  jdfFreeAsInterface:
    try
      IUnknown(AData)._Release;
    except
    end;
end;
end;

function TQWorkers.GetEnabled: Boolean;
begin
Result := (FDisableCount = 0);
end;

function TQWorkers.LongtimeJob(AProc: TQJobProc; AData: Pointer;
  AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
if AtomicIncrement(FLongTimeWorkers) <= FMaxLongtimeWorkers then
  begin
  Result := true;
  AJob := JobPool.Pop;
  JobInitialize(AJob, AData, AFreeType, true, False);
  AJob.SetFlags(JOB_LONGTIME,True);
  AJob.WorkerProc := AProc;
  Result := Post(AJob);
  if not Result then
    JobPool.Push(AJob);
  end
else
  begin
  AtomicDecrement(FLongTimeWorkers);
  Result := False;
  end;
end;
{$IFDEF UNICODE}

function TQWorkers.LongtimeJob(AProc: TQJobProcA; AData: Pointer;
  AFreeType: TQJobDataFreeType = jdfFreeByUser): Boolean;
var
  AJob: PQJob;
begin
if AtomicIncrement(FLongTimeWorkers) <= FMaxLongtimeWorkers then
  begin
  Result := true;
  AJob := JobPool.Pop;
  JobInitialize(AJob, AData, AFreeType, true, False);
  AJob.SetFlags(JOB_LONGTIME,True);
  AJob.WorkerProcA := AProc;
  AJob.IsAnonWorkerProc := true;
  Result := Post(AJob);
  if not Result then
    JobPool.Push(AJob);
  end
else
  begin
  AtomicDecrement(FLongTimeWorkers);
  Result := False;
  end;
end;
{$ENDIF}

function TQWorkers.LongtimeJob(AProc: TQJobProcG; AData: Pointer;
  AFreeType: TQJobDataFreeType): Boolean;
begin
Result := LongtimeJob(MakeJobProc(AProc), AData, AFreeType);
end;

function TQWorkers.LookupIdleWorker: Boolean;
var
  i: Integer;
  AWorker: TQWorker;
begin
Result := False;
if (FDisableCount <> 0) or (FBusyCount = MaxWorkers) or FTerminating then
  Exit;
FLocker.Enter;
try
  if FBusyCount < FWorkerCount then
    begin
    for i := 0 to FWorkerCount - 1 do
      begin
      AWorker := FWorkers[i];
      if (AWorker <> nil) and (AWorker.IsIdle) then
        begin
        AWorker.Suspended := False;
        AWorker.SetFlags(WORKER_ISBUSY, true);
        AtomicIncrement(FBusyCount);
        AWorker.FEvent.SetEvent;
        Result := true;
        Exit;
        end;
      end;
    end;
  if (not Result) and (FWorkerCount < MaxWorkers) then
    begin
    AWorker := TQWorker.Create(Self);
    SetThreadCPU(AWorker.Handle, FWorkerCount mod GetCPUCount);
    AWorker.Suspended := False;
    AWorker.FEvent.SetEvent;
    FWorkers[FWorkerCount] := AWorker;
    Inc(FWorkerCount);
    Result := true;
    end;
finally
  FLocker.Leave;
end;
end;

function TQWorkers.Popup: PQJob;
begin
Result := FSimpleJobs.Pop;
if Result = nil then
  Result := FRepeatJobs.Pop;
end;

function TQWorkers.RegisterSignal(const AName: QStringW): Integer;
var
  ASignal: PQSignal;
begin
FLocker.Enter;
try
  Result := SignalIdByName(AName);
  if Result < 0 then
    begin
    Inc(FMaxSignalId);
    New(ASignal);
    ASignal.Id := FMaxSignalId;
    ASignal.Fired := 0;
    ASignal.Name := AName;
    ASignal.First := nil;
    FSignalJobs.Add(ASignal, ASignal.Id);
    Result := ASignal.Id;
    // OutputDebugString(PWideChar('Signal '+IntToStr(ASignal.Id)+' Allocate '+IntToHex(NativeInt(ASignal),8)));
    end;
finally
  FLocker.Leave;
end;
end;

procedure TQWorkers.SetEnabled(const Value: Boolean);
begin
if Value then
  EnableWorkers
else
  DisableWorkers;
end;

procedure TQWorkers.SetMaxLongtimeWorkers(const Value: Integer);
begin
if FMaxLongtimeWorkers <> Value then
  begin
  if Value > (MaxWorkers shr 1) then
    raise Exception.Create(STooManyLongtimeWorker);
  FMaxLongtimeWorkers := Value;
  end;
end;

procedure TQWorkers.SetMaxWorkers(const Value: Integer);
var
  ATemp, AMaxLong: Integer;
begin
if (Value >= 2) and (FMaxWorkers <> Value) then
  begin
  AtomicExchange(ATemp, FLongTimeWorkers);
  AtomicExchange(FLongTimeWorkers, 0); // 强制置0，防止有新入的长时间作业
  AMaxLong := Value shr 1;
  FLocker.Enter;
  try
    if FLongTimeWorkers < AMaxLong then // 已经进行的长时间作业数小于一半的工作者
      begin
      if ATemp < AMaxLong then
        AMaxLong := ATemp;
      if FMaxWorkers > Value then
        begin
        while Value < FWorkerCount do
          WorkerTerminate(FWorkers[FWorkerCount - 1]);
        FMaxWorkers := Value;
        SetLength(FWorkers, Value);
        end
      else
        begin
        FMaxWorkers := Value;
        SetLength(FWorkers, Value);
        end;
      end;
  finally
    FLocker.Leave;
    AtomicExchange(FLongTimeWorkers, AMaxLong);
  end;
  end;
end;

procedure TQWorkers.SetMinWorkers(const Value: Integer);
begin
if FMinWorkers <> Value then
  begin
  if Value < 1 then
    raise Exception.Create(STooFewWorkers);
  FMinWorkers := Value;
  end;
end;

procedure TQWorkers.Signal(AId: Integer; AData: Pointer;
  AFreeType: TQJobDataFreeType);
var
  AFound: Boolean;
  ASignal: PQSignal;
begin
AFound := False;
FLocker.Enter;
try
  ASignal := FSignalJobs.FindFirstData(AId);
  if ASignal <> nil then
    begin
    AFound := true;
    FireSignalJob(ASignal, AData, AFreeType);
    end
  else
    FreeJobData(AData, AFreeType);
finally
  FLocker.Leave;
end;
if AFound then
  LookupIdleWorker;
end;

procedure TQWorkers.Signal(const AName: QStringW; AData: Pointer;
  AFreeType: TQJobDataFreeType);
var
  i: Integer;
  ASignal: PQSignal;
  AFound: Boolean;
begin
AFound := False;
FLocker.Enter;
try
  for i := 0 to FSignalJobs.BucketCount - 1 do
    begin
    if FSignalJobs.Buckets[i] <> nil then
      begin
      ASignal := FSignalJobs.Buckets[i].Data;
      if (Length(ASignal.Name) = Length(AName)) and (ASignal.Name = AName) then
        begin
        AFound := true;
        FireSignalJob(ASignal, AData, AFreeType);
        Break;
        end;
      end;
    end;
finally
  FLocker.Leave;
end;
if AFound then
  LookupIdleWorker
else
  FreeJobData(AData, AFreeType);
end;

function TQWorkers.SignalIdByName(const AName: QStringW): Integer;
var
  i: Integer;
  ASignal: PQSignal;
begin
Result := -1;
for i := 0 to FSignalJobs.BucketCount - 1 do
  begin
  if FSignalJobs.Buckets[i] <> nil then
    begin
    ASignal := FSignalJobs.Buckets[i].Data;
    if (Length(ASignal.Name) = Length(AName)) and (ASignal.Name = AName) then
      begin
      Result := ASignal.Id;
      Exit;
      end;
    end;
  end;
end;

procedure TQWorkers.SignalWorkDone(AJob: PQJob; AUsedTime: Int64);
var
  ASignal: PQSignal;
  ATemp, APrior: PQJob;
begin
FLocker.Enter;
try
  ASignal := FSignalJobs.FindFirstData(AJob.SignalId);
  ATemp := ASignal.First;
  APrior := nil;
  while ATemp <> nil do
    begin
    if ATemp = AJob.Source then
      begin
      if AJob.IsTerminated then
        begin
        if APrior <> nil then
          APrior.Next := ATemp.Next
        else
          ASignal.First := ATemp.Next;
        ATemp.Next := nil;
        FreeJob(ATemp);
        end
      else
        begin
        // 更新信号作业的统计信息
        Inc(ATemp.Runs);
        if AUsedTime > 0 then
          begin
          if ATemp.MinUsedTime = 0 then
            ATemp.MinUsedTime := AUsedTime
          else if AUsedTime < ATemp.MinUsedTime then
            ATemp.MinUsedTime := AUsedTime;
          if ATemp.MaxUsedTime = 0 then
            ATemp.MaxUsedTime := AUsedTime
          else if AUsedTime > ATemp.MaxUsedTime then
            ATemp.MaxUsedTime := AUsedTime;
          Break;
          end;
        end;
      end;
    APrior := ATemp;
    ATemp := ATemp.Next;
    end;
finally
  FLocker.Leave;
end;
end;

procedure TQWorkers.WorkerBusy(AWorker: TQWorker);
begin
AtomicIncrement(FBusyCount);
end;

procedure TQWorkers.WorkerIdle(AWorker: TQWorker; AReason: TWorkerIdleReason);
var
  i, J: Integer;
begin
if AtomicDecrement(FBusyCount) > FMinWorkers then
  begin
  FLocker.Enter;
  try
    if (AWorker <> FWorkers[0]) and (AWorker <> FWorkers[1]) and
      (AReason = irTimeout) then
      begin
      for i := FMinWorkers to FWorkerCount - 1 do
        begin
        if AWorker = FWorkers[i] then
          begin
          AWorker.Terminate;
          for J := i + 1 to FWorkerCount - 1 do
            FWorkers[J - 1] := FWorkers[J];
          FWorkers[FWorkerCount - 1] := nil;
          Dec(FWorkerCount);
          Break;
          end;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  end;
end;

procedure TQWorkers.WorkerTerminate(AWorker: TObject);
var
  i, J: Integer;
begin
AtomicDecrement(FBusyCount);
FLocker.Enter;
for i := 0 to FWorkerCount - 1 do
  begin
  if FWorkers[i] = AWorker then
    begin
    for J := i to FWorkerCount - 2 do
      FWorkers[J] := FWorkers[J + 1];
    FWorkers[FWorkerCount - 1] := nil;
    Dec(FWorkerCount);
    Break;
    end;
  end;
FLocker.Leave;
// PostLog(llHint,'工作者 %d 结束，新数量 %d',[TQWorker(AWorker).ThreadID,FWorkerCount]);
end;

function TQWorkers.Wait(AProc: TQJobProc; ASignalId: Integer;
  ARunInMainThread: Boolean): Boolean;
var
  AJob: PQJob;
  ASignal: PQSignal;
begin
if not FTerminating then
  begin
  AJob := JobPool.Pop;
  JobInitialize(AJob, nil, jdfFreeByUser, False, ARunInMainThread);
  AJob.WorkerProc := AProc;
  AJob.SignalId := ASignalId;
  AJob.SetFlags(JOB_SIGNAL_WAKEUP, true);
  AJob.PushTime := GetTimestamp;
  Result := False;
  FLocker.Enter;
  try
    ASignal := FSignalJobs.FindFirstData(ASignalId);
    if ASignal <> nil then
      begin
      AJob.Next := ASignal.First;
      ASignal.First := AJob;
      Result := true;
      end;
  finally
    FLocker.Leave;
    if not Result then
      JobPool.Push(AJob);
  end;
  end
else
  Result := False;
end;
{$IFDEF UNICODE}

function TQWorkers.Wait(AProc: TQJobProcA; ASignalId: Integer;
  ARunInMainThread: Boolean): Boolean;
var
  AJob: PQJob;
  ASignal: PQSignal;
begin
if not FTerminating then
  begin
  AJob := JobPool.Pop;
  JobInitialize(AJob, nil, jdfFreeByUser, False, ARunInMainThread);
  AJob.WorkerProcA := AProc;
  AJob.IsAnonWorkerProc := true;
  AJob.SignalId := ASignalId;
  AJob.SetFlags(JOB_SIGNAL_WAKEUP, true);
  AJob.PushTime := GetTimestamp;
  Result := False;
  FLocker.Enter;
  try
    ASignal := FSignalJobs.FindFirstData(ASignalId);
    if ASignal <> nil then
      begin
      AJob.Next := ASignal.First;
      ASignal.First := AJob;
      Result := true;
      end;
  finally
    FLocker.Leave;
    if not Result then
      JobPool.Push(AJob);
  end;
  end
else
  Result := False;

end;
{$ENDIF}

function TQWorkers.Wait(AProc: TQJobProcG; ASignalId: Integer;
  ARunInMainThread: Boolean): Boolean;
begin
Result := Wait(MakeJobProc(AProc), ASignalId, ARunInMainThread);
end;

procedure TQWorkers.WaitRunningDone(const AParam: TWorkerWaitParam);
var
  AInMainThread: Boolean;
  function HasJobRunning: Boolean;
  var
    i: Integer;
    AJob: PQJob;
  begin
  Result := False;
  DisableWorkers;
  FLocker.Enter;
  try
    for i := 0 to FWorkerCount - 1 do
      begin
      if FWorkers[i].IsLookuping then // 还未就绪，所以在下次查询
        begin
        Result := true;
        Break;
        end
      else if FWorkers[i].IsExecuting then
        begin
        AJob := FWorkers[i].FActiveJob;
        case AParam.WaitType of
          0: // ByObject
            Result := TMethod(FWorkers[i].FActiveJobProc).Data = AParam.Bound;
          1: // ByData
            Result := (TMethod(FWorkers[i].FActiveJobProc)
              .Code = TMethod(AParam.WorkerProc).Code) and
              (TMethod(FWorkers[i].FActiveJobProc)
              .Data = TMethod(AParam.WorkerProc).Data) and
              ((AParam.Data = Pointer(-1)) or
              (FWorkers[i].FActiveJobData = AParam.Data));
          2: // BySignalSource
            Result := (FWorkers[i].FActiveJobSource = AParam.SourceJob);
          3: // ByGroup
            Result := (FWorkers[i].FActiveJobGroup = AParam.Group);
          $FF: // 所有
            Result := true;
        else
          raise Exception.CreateFmt(SBadWaitDoneParam, [AParam.WaitType]);
        end;
        if Result then
          begin
          FWorkers[i].FTerminatingJob := AJob;
          Break;
          end;
        end;
      end;
  finally
    FLocker.Leave;
    EnableWorkers;
  end;
  end;

begin
AInMainThread := GetCurrentThreadId = MainThreadId;
repeat
  if HasJobRunning then
    begin
    if AInMainThread then
      begin
      // 如果是在主线程中清理，由于作业可能在主线程执行，可能已经投寄尚未执行，所以必需让其能够执行
{$IFDEF NEXTGEN}
      fmx.Forms.Application.ProcessMessages;
{$ELSE}
      Forms.Application.ProcessMessages;
{$ENDIF}
      end;
{$IFDEF MSWINDOWS}
    SwitchToThread;
{$ELSE}
    TThread.Yield;
{$ENDIF}
    end
  else // 没找到
    Break;
until 1 > 2;
end;

procedure TQWorkers.WaitSignalJobsDone(AJob: PQJob);
begin
TEvent(AJob.Data).SetEvent;
end;

function TQWorkers.Clear(ASignalName: QStringW): Integer;
var
  i: Integer;
  ASignal: PQSignal;
  AJob: PQJob;
begin
Result := 0;
FLocker.Enter;
try
  AJob := nil;
  for i := 0 to FSignalJobs.BucketCount - 1 do
    begin
    if FSignalJobs.Buckets[i] <> nil then
      begin
      ASignal := FSignalJobs.Buckets[i].Data;
      if ASignal.Name = ASignalName then
        begin
        AJob := ASignal.First;
        ASignal.First := nil;
        Break;
        end;
      end;
    end;
finally
  FLocker.Leave;
end;
if AJob <> nil then
  ClearSignalJobs(AJob);
end;
{$IFDEF UNICODE}

function TQWorkers.At(AProc: TQJobProcA; const ADelay, AInterval: Int64;
  AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.WorkerProcA := AProc;
AJob.IsAnonWorkerProc := true;
AJob.Interval := AInterval;
AJob.FirstDelay := ADelay;
Result := Post(AJob);
end;
{$ENDIF}

function TQWorkers.At(AProc: TQJobProcG; const ADelay, AInterval: Int64;
  AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
begin
Result := At(MakeJobProc(AProc), ADelay, AInterval, AData, ARunInMainThread,
  AFreeType);
end;

function TQWorkers.At(AProc: TQJobProcG; const ATime: TDateTime;
  const AInterval: Int64; AData: Pointer; ARunInMainThread: Boolean;
  AFreeType: TQJobDataFreeType): Boolean;
begin
Result := At(MakeJobProc(AProc), ATime, AInterval, AData, ARunInMainThread,
  AFreeType);
end;

function TQWorkers.Clear(ASignalId: Integer): Integer;
var
  i: Integer;
  ASignal: PQSignal;
  AJob: PQJob;
begin
FLocker.Enter;
try
  AJob := nil;
  for i := 0 to FSignalJobs.BucketCount - 1 do
    begin
    if FSignalJobs.Buckets[i] <> nil then
      begin
      ASignal := FSignalJobs.Buckets[i].Data;
      if ASignal.Id = ASignalId then
        begin
        AJob := ASignal.First;
        ASignal.First := nil;
        Break;
        end;
      end;
    end;
finally
  FLocker.Leave;
end;
if AJob <> nil then
  Result := ClearSignalJobs(AJob)
else
  Result := 0;
end;

procedure TQWorkers.Clear;
var
  i: Integer;
  AParam: TWorkerWaitParam;
  ASignal: PQSignal;
begin
DisableWorkers; // 避免工作者取得新的作业
try
  FSimpleJobs.Clear;
  FRepeatJobs.Clear;
  FLocker.Enter;
  try
    for i := 0 to FSignalJobs.BucketCount - 1 do
      begin
      if Assigned(FSignalJobs.Buckets[i]) then
        begin
        ASignal := FSignalJobs.Buckets[i].Data;
        FreeJob(ASignal.First);
        ASignal.First := nil;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  AParam.WaitType := $FF;
  WaitRunningDone(AParam);
finally
  EnableWorkers;
end;
end;

function TQWorkers.ClearSignalJobs(ASource: PQJob): Integer;
var
  AFirst, ALast, APrior, ANext: PQJob;
  ACount: Integer;
  AWaitParam: TWorkerWaitParam;
begin
Result := 0;
AFirst := nil;
APrior := nil;
FSimpleJobs.FLocker.Enter;
try
  ALast := FSimpleJobs.FFirst;
  ACount := FSimpleJobs.Count;
  FSimpleJobs.FFirst := nil;
  FSimpleJobs.FLast := nil;
  FSimpleJobs.FCount := 0;
finally
  FSimpleJobs.FLocker.Leave;
end;
while ALast <> nil do
  begin
  if (ALast.IsSignalWakeup) and (ALast.Source = ASource) then
    begin
    ANext := ALast.Next;
    ALast.Next := nil;
    FreeJob(ALast);
    ALast := ANext;
    if APrior <> nil then
      APrior.Next := ANext;
    Dec(ACount);
    Inc(Result);
    end
  else
    begin
    if AFirst = nil then
      AFirst := ALast;
    APrior := ALast;
    ALast := ALast.Next;
    end;
  end;
if ACount > 0 then
  begin
  FSimpleJobs.FLocker.Enter;
  try
    APrior.Next := FSimpleJobs.FFirst;
    FSimpleJobs.FFirst := AFirst;
    Inc(FSimpleJobs.FCount, ACount);
    if FSimpleJobs.FLast = nil then
      FSimpleJobs.FLast := APrior;
  finally
    FSimpleJobs.FLocker.Leave;
  end;
  end;
AWaitParam.WaitType := 2;
AWaitParam.SourceJob := ASource;
WaitRunningDone(AWaitParam);
FreeJob(ASource);
end;
{$IFDEF UNICODE}

function TQWorkers.Post(AProc: TQJobProcA; AInterval: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, AInterval = 0, ARunInMainThread);
AJob.WorkerProcA := AProc;
AJob.IsAnonWorkerProc := true;
AJob.Interval := AInterval;
Result := Post(AJob);
end;
{$ENDIF}

function TQWorkers.Post(AProc: TQJobProcG; AInterval: Int64; AData: Pointer;
  ARunInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
begin
Result := Post(MakeJobProc(AProc), AInterval, AData, ARunInMainThread,
  AFreeType);
end;

{ TJobPool }

constructor TJobPool.Create(AMaxSize: Integer);
var
  i: Integer;
begin
inherited Create;
FSize := AMaxSize;
FLocker := TQSimpleLock.Create;
end;

destructor TJobPool.Destroy;
var
  AJob: PQJob;
begin
FLocker.Enter;
while FFirst <> nil do
  begin
  AJob := FFirst.Next;
  Dispose(FFirst);
  FFirst := AJob;
  end;
FreeObject(FLocker);
inherited;
end;

function TJobPool.Pop: PQJob;
begin
FLocker.Enter;
Result := FFirst;
if Result <> nil then
  begin
  FFirst := Result.Next;
  Dec(FCount);
  end;
FLocker.Leave;
if Result = nil then
  GetMem(Result, SizeOf(TQJob));
Result.Reset;
end;

procedure TJobPool.Push(AJob: PQJob);
var
  ADoFree: Boolean;
begin
{$IFDEF UNICODE}
if AJob.IsAnonWorkerProc then
  AJob.WorkerProcA := nil;
{$ENDIF}
FLocker.Enter;
ADoFree := (FCount = FSize);
if not ADoFree then
  begin
  AJob.Next := FFirst;
  FFirst := AJob;
  Inc(FCount);
  end;
FLocker.Leave;
if ADoFree then
  begin
  FreeMem(AJob);
  end;
end;

{ TQSimpleLock }
{$IFDEF QWORKER_SIMPLE_LOCK}

constructor TQSimpleLock.Create;
begin
inherited;
FFlags := 0;
end;

procedure TQSimpleLock.Enter;
begin
while (AtomicOr(FFlags, $01) and $01) <> 0 do
  begin
{$IFDEF MSWINDOWS}
  SwitchToThread;
{$ELSE}
  TThread.Yield;
{$ENDIF}
  end;
end;

procedure TQSimpleLock.Leave;
begin
AtomicAnd(FFlags, Integer($FFFFFFFE));
end;
{$ENDIF QWORKER_SIMPLE_JOB}
{ TQJobGroup }

function TQJobGroup.Add(AProc: TQJobProc; AData: Pointer;
  AInMainThread: Boolean; AFreeType: TQJobDataFreeType): Boolean;
var
  AJob: PQJob;
begin
AJob := JobPool.Pop;
JobInitialize(AJob, AData, AFreeType, true, AInMainThread);
AJob.Group := Self;
AJob.WorkerProc := AProc;
AJob.SetFlags(JOB_GROUPED, true);
FLocker.Enter;
try
  FWaitResult := wrIOCompletion;
  if FPrepareCount > 0 then // 正在添加项目，加到列表中，等待Run
    begin
    FItems.Add(AJob);
    Result := true;
    end
  else
    begin
    if ByOrder then // 按顺序
      begin
      Result := true;
      FItems.Add(AJob);
      if FItems.Count = 0 then
        Result := Workers.Post(AJob);
      end
    else
      begin
      Result := Workers.Post(AJob);
      if Result then
        FItems.Add(AJob);
      end;
    end;
finally
  FLocker.Leave;
end;
end;

procedure TQJobGroup.Cancel;
var
  i: Integer;
  AJobs: TQSimpleJobs;
  AJob, APrior, ANext: PQJob;
  AWaitParam: TWorkerWaitParam;
begin
FLocker.Enter;
try
  if FByOrder then
    begin
    for i := 0 to FItems.Count - 1 do
      begin
      AJob := FItems[i];
      if AJob.PopTime=0 then
        Workers.FreeJob(AJob);
      end;
    end;
  FItems.Clear;
finally
  FLocker.Leave;
end;
// 从SimpleJobs里清除关联的全部作业
AJobs := Workers.FSimpleJobs;
AJobs.FLocker.Enter;
try
  AJob := AJobs.FFirst;
  APrior := nil;
  while AJob <> nil do
    begin
    ANext := AJob.Next;
    if AJob.IsGrouped and (AJob.Group = Self) then
      begin
      if APrior = nil then
        AJobs.FFirst := AJob.Next
      else
        APrior.Next := AJob.Next;
      AJob.Next := nil;
      Workers.FreeJob(AJob);
      if AJob = AJobs.FLast then
        AJobs.FLast := nil;
      end
    else
      APrior := AJob;
    AJob := ANext;
    end;
finally
  AJobs.FLocker.Leave;
end;
AWaitParam.WaitType := 3;
AWaitParam.Group := Self;
Workers.WaitRunningDone(AWaitParam);
end;

constructor TQJobGroup.Create(AByOrder: Boolean);
begin
inherited Create;
FEvent := TEvent.Create(nil, False, False, '');
FLocker := TQSimpleLock.Create;
FByOrder := AByOrder;
FItems := TQJobItemList.Create;
end;

destructor TQJobGroup.Destroy;
var
  i: Integer;
begin
Cancel;
Workers.FSimpleJobs.Clear(Self, -1);
FLocker.Enter;
try
  if FItems.Count > 0 then
    begin
    FWaitResult := wrAbandoned;
    FEvent.SetEvent;
    for i := 0 to FItems.Count - 1 do
      begin
      if PQJob(FItems[i]).PushTime <> 0 then
        JobPool.Push(FItems[i]);
      end;
    FItems.Clear;
    end;
finally
  FLocker.Leave;
end;
FreeObject(FLocker);
FreeObject(FEvent);
FreeObject(FItems);
inherited;
end;

procedure TQJobGroup.DoAfterDone;
begin
if Assigned(FAfterDone) then
  FAfterDone(Self);
end;

procedure TQJobGroup.DoJobExecuted(AJob: PQJob);
var
  i: Integer;
  AIsDone: Boolean;
begin
if FWaitResult = wrIOCompletion then
  begin
  AIsDone := False;
  FLocker.Enter;
  try
    i := FItems.IndexOf(AJob);
    if i <> -1 then
      begin
      FItems.Delete(i);
      if FItems.Count = 0 then
        begin
        FWaitResult := wrSignaled;
        FEvent.SetEvent;
        AIsDone := true;
        end
      else if ByOrder then
        begin
        if not Workers.Post(FItems[0]) then
          begin
          FWaitResult := wrAbandoned;
          FEvent.SetEvent;
          end;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  if AIsDone then
    DoAfterDone;
  end;
end;

procedure TQJobGroup.DoJobsTimeout(AJob: PQJob);
begin
Cancel;
if FWaitResult = wrIOCompletion then
  begin
  FWaitResult := wrTimeout;
  FEvent.SetEvent;
  DoAfterDone;
  end;
end;

function TQJobGroup.MsgWaitFor(ATimeout: Cardinal): TWaitResult;
var
  AEndTime: Int64;
begin
if GetCurrentThreadId <> MainThreadId then
  Result := WaitFor(ATimeout)
else
  begin
  Result := FWaitResult;
  FLocker.Enter;
  try
    if FItems.Count = 0 then
      Result := wrSignaled;
  finally
    FLocker.Leave;
  end;
  if Result = wrIOCompletion then
    begin
    AEndTime := GetTimestamp + ATimeout * 10;
    while GetTimestamp < AEndTime do
      begin
      // 每隔10毫秒检查一下是否有消息需要处理，有则处理，无则进入下一个等待
      if FEvent.WaitFor(10) = wrSignaled then
        begin
        Result := FWaitResult;
        Break;
        end
      else
        begin
        // 如果是在主线程中清理，由于作业可能在主线程执行，可能已经投寄尚未执行，所以必需让其能够执行
{$IFDEF NEXTGEN}
        fmx.Forms.Application.ProcessMessages;
{$ELSE}
        Forms.Application.ProcessMessages;
{$ENDIF}
        end;
      end;
    if Result = wrIOCompletion then
      begin
      Cancel;
      if Result = wrIOCompletion then
        Result := wrTimeout;
      end;
    DoAfterDone;
    end;
  end;

end;

procedure TQJobGroup.Prepare;
begin
AtomicIncrement(FPrepareCount);
end;

procedure TQJobGroup.Run(ATimeout: Cardinal);
var
  i: Integer;
begin
if AtomicDecrement(FPrepareCount) = 0 then
  begin
  if ATimeout <> INFINITE then
    begin
    FTimeout := GetTimestamp - ATimeout;
    Workers.Delay(DoJobsTimeout, ATimeout * 10, nil);
    end;
  FLocker.Enter;
  try
    if FItems.Count = 0 then
      FWaitResult := wrSignaled
    else
      begin
      FWaitResult := wrIOCompletion;
      if ByOrder then
        begin
        if not Workers.Post(FItems[0]) then
          FWaitResult := wrAbandoned;
        end
      else
        begin
        for i := 0 to FItems.Count - 1 do
          begin
          if not Workers.Post(FItems[i]) then
            begin
            FWaitResult := wrAbandoned;
            Break;
            end;
          end;
        end;
      end;
  finally
    FLocker.Leave;
  end;
  if FWaitResult <> wrIOCompletion then
    DoAfterDone;
  end;
end;

function TQJobGroup.WaitFor(ATimeout: Cardinal): TWaitResult;
begin
Result := FWaitResult;
FLocker.Enter;
try
  if FItems.Count = 0 then
    Result := wrSignaled;
finally
  FLocker.Leave;
end;
if Result = wrIOCompletion then
  begin
  if FEvent.WaitFor(ATimeout) = wrSignaled then
    Result := FWaitResult
  else
    Result := wrTimeout;
  end;
DoAfterDone;
end;

function JobPoolCount: NativeInt;
begin
Result := JobPool.Count;
end;
function JobPoolPrint: QStringW;
var
  AJob:PQJob;
  ABuilder:TQStringCatHelperW;
begin
ABuilder:=TQStringCatHelperW.Create;
JobPool.FLocker.Enter;
try
  AJob:=JobPool.FFirst;
  while AJob<>nil do
    begin
    ABuilder.Cat(IntToHex(NativeInt(AJob),SizeOf(NativeInt))).Cat(SLineBreak);
    AJob:=AJob.Next;
    end;
finally
  JobPool.FLocker.Leave;
  Result:=ABuilder.Value;
  FreeObject(ABuilder);
end;
end;

initialization

{$IFNDEF NEXTGEN}
  GetTickCount64 := GetProcAddress(GetModuleHandle(kernel32), 'GetTickCount64');
if not QueryPerformanceFrequency(_PerfFreq) then
  _PerfFreq := -1;
{$ELSE}
  _Watch := TStopWatch.Create;
_Watch.Start;
{$ENDIF}
JobPool := TJobPool.Create(1024);
Workers := TQWorkers.Create;

finalization

FreeObject(Workers);
FreeObject(JobPool);

end.
