unit qlog;

interface
{$I 'qdac.inc'}
// QLOG_CREATE_GLOBAL指定是否创建默认的QLogs全局变量，如果定义，则创建默认的QLogs对象
{$DEFINE QLOG_CREATE_GLOBAL}
{$IFNDEF NEXTGEN}
  {$DEFINE QLOG_CREATE_DEFAULT_WRITER}
{$ENDIF !NEXTGEN}
uses classes,SysUtils,qstring,SyncObjs{$IFDEF UNICODE},ZLib{$ENDIF}
  {$IFDEF POSIX}
  ,Posix.Base,Posix.Stdio,Posix.Pthread,Posix.UniStd,IOUtils,Posix.NetDB,Posix.SysSocket,
  Posix.NetinetIn,Posix.arpainet,Posix.SysSelect,Posix.Systime
  {$ELSE}
  ,Windows,winsock
  {$ENDIF};
{
  本源码来自QDAC项目，版权归swish(QQ:109867294)所有。
  (1)、使用许可及限制
  您可以自由复制、分发、修改本源码，但您的修改应该反馈给作者，并允许作者在必要时，
合并到本项目中以供使用，合并后的源码同样遵循QDAC版权声明限制。
  您的产品的关于中，应包含以下的版本声明:
  本产品使用的日志记录器来自QDAC项目中的QLog，版权归作者所有。
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

(*
QLog日志记录单元
【注意】
  XE6自带的zlib支持了gzwrite等函数，而Delphi 2007自带的zlib不支持，因此，QLog在
Delphi 2007编译的程序上运行时，请携带zlib1.dll，而XE6编译的版本不需要。
【QLog是什么】
  本单元用于提供一个高性能的日志记录单元，使用简单的接口完成日志记录工作。对上层
隐藏内部实现逻辑：
  1、外部正常只需要调用PostLog函数(C++ Builder用户还可以调用AddLog)就可以完成日
志的记录.
  2、子类可以继承实现各种输入(TQLogWriter)和日志读取接口(TQLogReader)，但目前版
本只实现了TQLogFileWriter和TQLogSocketWriter。
  本单元类的集成和继承关系如下：
  集成：
    TQLog <- TQLogCastor <- TQLogWriter
  继承：
    TQLog
    TQLogWriter->TQLogFileWriter
        +->TQLogSocketWriter
    TQLogReader
  本单元各个类的作用：
  TQLog : 日志缓存管理单元，用于缓存需要记录的日志
  TQLogCastor : 日志后台写入线程，负责将日志实际格式化并调用各个TQLogWriter实例完成写入
  TQLogWriter : 实际的日志写入对象
  TQLogReader : 日志读取对象，用于日志的定位和获取
  TQLogFileWriter : 日志文件写入对象
  TQLogSocketWriter : 日志文件syslog支持对象，可以发送日志到syslog服务器或其它监听程序 
*)
{修订日志
=========
2014.8.2
  * 修正了CreateItem时如果日志内容为空时，访问越界的问题
2014.6.26
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)
2014.6.21
  * 修正了2010中由于TThreadId没定义造成无法编译的问题
}
{$M+}
{$HPPEMIT '#pragma link "qlog"'}
type
  {日志记录模式
  lmSync : 同步模式，等待日志写入完成
  lmAsyn : 异步模式，不等待日志写入完成
  }
  TQLogMode=(lmSync,lmAsyn);
{
C++:
  由于$HPPEMIT指令的命令被强制放在了#include之后，其它代码之前，因此由于TLogLevel
未声明，造成无法编译，因此使用模板和宏来处理下，以便滞后编译
}
{$HPPEMIT '#include <stdio.h>'}
{$HPPEMIT 'template<class TLogLevel> void PostLog(TLogLevel ALevel,const wchar_t *format,...)'}
{$HPPEMIT '{'}
{$HPPEMIT 'int ASize;'}
{$HPPEMIT 'QStringW AMsg;'}
{$HPPEMIT 'va_list args;'}
{$HPPEMIT 'va_start(args, format);}
{$HPPEMIT 'ASize=vsnwprintf(NULL, 0, format, args);}
{$HPPEMIT 'AMsg.SetLength(ASize);'}
{$IFDEF UNICODE}
{$HPPEMIT 'vsnwprintf(AMsg.c_str(), ASize+1, format, args);'}
{$ELSE}
{$HPPEMIT 'vsnwprintf(AMsg.c_bstr(),ASize+1,format,args);'}
{$ENDIF}
{$HPPEMIT 'PostLog(ALevel,AMsg);'}
{$HPPEMIT 'va_end(args);'}
(*$HPPEMIT '}'*)
{$HPPEMIT '#define AddLog PostLog<TQLogLevel>'}
  {
  //兼容下Linux的Syslog日志级别定义
    0       Emergency: system is unusable
    1       Alert: action must be taken immediately
    2       Critical: critical conditions
    3       Error: error conditions
    4       Warning: warning conditions
    5       Notice: normal but significant condition
    6       Informational: informational messages
    7       Debug: debug-level messages
  }

  TQLogLevel=(llEmergency,llAlert,llFatal,llError,llWarning,llHint,llMessage,llDebug);
  TQLog=class;
  TQLogCastor=class;
  TQLogWriter=class;
  TQLogReader=class;
  {$IF RTLVersion<22}
  TThreadId=LongWord;
  {$IFEND}
  //日志记录条目
  PQLogItem=^TQLogItem;
  TQLogItem=record
    Next,Prior:PQLogItem;
    ThreadId:TThreadID;
    TimeStamp:TDateTime;
    Level:TQLogLevel;
    MsgLen:Integer;
    Text:array[0..0] of WideChar;
  end;

  TQLogList=record
  case Integer of
    0:(Value:Int64;);
    1:(
      First:PQLogItem;
      Last:PQLogItem;
      );
    2:
      (
      FirstVal:Integer;
      LastVal:Integer;
      );
  end;
  
  //日志缓存
  TQLog=class
  protected
    FList:TQLogList;
    FCastor:TQLogCastor;
    FInFree:Boolean;
    FCount:Integer;
    FFlushed:Integer;
    FFlags:Integer;
    FCS:TCriticalSection;
    procedure Lock;
    procedure Unlock;
    function Pop:PQLogItem;
    function CreateCastor:TQLogCastor;virtual;
    function GetCastor:TQLogCastor;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Post(ALevel:TQLogLevel;const AMsg:QStringW);overload;
    procedure Post(ALevel:TQLogLevel;const AFormat:QStringW;Args:array of const);overload;
    property Castor:TQLogCastor read GetCastor;
    property Count:Integer read FCount;
    property Flushed:Integer read FFlushed;
  end;

  //日志写入对象
  TQLogWriter=class
  protected
    FCastor:TQLogCastor;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure HandleNeeded;virtual;
    function WriteItem(AItem:PQLogItem):Boolean;virtual;
  end;

  //日志读取对象
  TQLogReader=class
  protected
    FItemIndex:Int64;
    function GetCount:Int64;virtual;
    procedure SetItemIndex(const Value: Int64);
  public
    constructor Create;overload;
    destructor Destroy;override;
    function ReadItem(var AMsg:QStringW;ALevel:TQLogLevel):Boolean;virtual;
    function First:Boolean;
    function Last:Boolean;
    function Prior:Boolean;
    function Next:Boolean;
    function MoveTo(AIndex:Int64):Boolean;virtual;
    property Count:Int64 read GetCount;
    property ItemIndex:Int64 read FItemIndex write SetItemIndex;
  end;
  PLogWriterItem=^TLogWriterItem;
  TLogWriterItem=record
    Next,Prior:PLogWriterItem;
    Writer:TQLogWriter;
  end;
  //日志广播对象，用于将日志传送到相应的对象，如本地系统文件或者是远程日志对象
  TQLogCastor=class(TThread)
  protected
    FLastError:Cardinal;
    FLastErrorMsg:QStringW;
    FNotifyHandle:TEvent;
    FCS:TCriticalSection;
    FOwner:TQLog;
    FWriters:PLogWriterItem;
    FActiveWriter:PLogWriterItem;
    FActiveLog: PQLogItem;
    procedure Execute;override;
    procedure LogAdded;
    function WaitForLog:Boolean;virtual;
    function FetchNext:PQLogItem;virtual;
    function FirstWriter:PLogWriterItem;
    function NextWriter:PLogWriterItem;
    {$IFNDEF UNICODE}
    function GetFinished: Boolean;
    {$ENDIF}
  published
  public
    constructor Create(AOwner:TQLog);overload;
    destructor Destroy;override;
    procedure SetLastError(ACode:Cardinal;const AMsg:QStringW);
    procedure AddWriter(AWriter:TQLogWriter);
    procedure RemoveWriter(AWriter:TQLogWriter);
    property ActiveLog:PQLogItem read FActiveLog;
    {$IFNDEF UNICODE}
    property Finished:Boolean read GetFinished;
    {$ENDIF}
  end;
  TQLogFileWriter=class(TQLogWriter)
  protected
    //索引和日志文件句柄
    FLogHandle,FIndexHandle:TFileStream;
    FFileName: QStringW;
    FIndexNeeded:Boolean;
    FBuffer:TBytes;
    FBuffered:Integer;
    FPosition:Int64;
    FLastTime:TDateTime;
    FLastThreadId:Cardinal;
    FLastTimeStamp,FLastThread:QStringW;
    FBuilder:TQStringCatHelperW;
    function FlushBuffer(AStream:TStream;p:Pointer;l:Integer):Boolean;
    procedure CompressLog(ALogFileName,AIndexFileName:QStringW);
  public
    /// 创建一个日志文件
    ///  <param name="AFileName">文件名</param>
    ///  <param name="AWithIndex">是否同时创建索引文件</param>
    ///  <remarks>
    ///  创建索引文件有利于检索日志时快速定位日志的起始位置，也可以不创建索引日志。
    /// 如果不创建索引，则定位到某一特定日志时，将需要更多的IO操作
    constructor Create(const AFileName:QStringW;AWithIndex:Boolean=False);overload;
    constructor Create;overload;
    destructor Destroy;override;
    function WriteItem(AItem:PQLogItem):Boolean;override;
    procedure HandleNeeded;override;
    property FileName:QStringW read FFileName;
  end;
  TQLogConsoleWriter=class(TQLogWriter)
  public
    constructor Create;overload;
    function WriteItem(AItem:PQLogItem):Boolean;override;
    procedure HandleNeeded;override;
  end;
  //Linux syslog服务写入
  TQLogSocketWriter=class(TQLogWriter)
  private
    FServerPort: Word;
    FServerHost: String;
    FSocket:THandle;
    FReaderAddr:sockaddr_in;
    FBuilder:TQStringCatHelperW;
    FTextEncoding: TTextEncoding;
    procedure SetTextEncoding(const Value: TTextEncoding);
  public
    constructor Create;overload;
    destructor Destroy;override;
    function WriteItem(AItem:PQLogItem):Boolean;override;
    procedure HandleNeeded;override;
    property ServerHost:String read FServerHost write FServerHost;
    property ServerPort:Word read FServerPort write FServerPort;
    property TextEncoding:TTextEncoding read FTextEncoding write SetTextEncoding;
  end;

procedure PostLog(ALevel:TQLogLevel;const AMsg:QStringW);overload;
procedure PostLog(ALevel:TQLogLevel;const fmt:PWideChar;args:array of const);overload;
{$IFDEF POSIX}
function GetCurrentProcessId:Integer;
{$ENDIF}
const
  ELOG_WRITE_FAILURE=$80000001;
var
  Logs:TQLog;
  MaxLogFileSize:Int64;
implementation
resourcestring
  SLogSeekError='无法定位到第%d条日志记录';
  SHandleNeeded='需要的日志写入对象句柄无法创建。';
  SCantCreateLogFile='无法创建指定的日志文件 "%s"。';
  SCantCreateCastor='无法创建日志广播对象。';
  SUnsupportSysLogEncoding='Syslog只支持Ansi和Utf8编码两种格式。';
  SZlibDLLMissed='zlib1.dll未找到，不支持压缩分卷日志。';
const
  SItemBreak:array[0..2] of WideChar=(#$3000,#13,#10);
  LogLevelText:array[llEmergency..llDebug] of QStringW=(
    '[EMG]','[ALERT]','[FATAL]','[ERROR]','[WARN]','[HINT]','[MSG]','[DEBUG]');

type
  TQLogCompressThread=class(TThread)
  protected
    FLogFileName,FIndexFileName:QStringW;
    procedure Execute;override;
  public
    constructor Create(ALogFileName,AIndexFileName:QStringW);overload;
  end;
{$IF RTLVersion<26}
  gzFile = Pointer;
  z_off_t = Longint;
  _gzopen=function (path: PAnsiChar; mode: PAnsiChar): gzFile; cdecl;
  _gzseek=function (file_: gzFile; offset: z_off_t; flush: Integer): z_off_t; cdecl;
  _gztell=function (file_: gzFile): z_off_t; cdecl;
  _gzwrite=function (file_: gzFile; const buf; len: Cardinal): Integer; cdecl;
  _gzclose=function (file_: gzFile): Integer; cdecl;
var
  gzopen:_gzopen;
  gzseek:_gzseek;
  gztell:_gztell;
  gzwrite:_gzwrite;
  gzclose:_gzclose;
  zlibhandle:THandle;  
{$IFEND <XE5UP}
{$IFDEF POSIX}

function GetCurrentProcessId:Integer;
begin
Result:=getpid;
end;
{$ENDIF}

procedure PostLog(ALevel:TQLogLevel;const AMsg:QStringW);
begin
Logs.Post(ALevel,AMsg);
end;
procedure PostLog(ALevel:TQLogLevel;const fmt:PWideChar;args:array of const);
begin
Logs.Post(ALevel,fmt,args);
end;

function CreateItemBuffer(ALevel:TQLogLevel;AMsgLen:Integer):PQLogItem;
begin
GetMem(Result,SizeOf(TQLogItem)+AMsgLen);
Result.Next:=nil;
Result.ThreadId:=GetCurrentThreadId;
Result.TimeStamp:=Now;
Result.Level:=ALevel;
Result.MsgLen:=AMsgLen;
end;

function CreateItem(const AMsg:QStringW;ALevel:TQLogLevel):PQLogItem;
var
  ALen:Integer;
begin
ALen:=Length(AMsg) shl 1;
Result:=CreateItemBuffer(ALevel,ALen);
if Result.MsgLen>0 then
  Move(PQCharW(AMsg)^,Result.Text[0],Result.MsgLen);
end;

procedure FreeItem(AItem:PQLogItem);
begin
FreeMem(AItem);
end;

//TQLogReader
constructor TQLogReader.Create;
begin

end;

destructor TQLogReader.Destroy;
begin

  inherited;
end;

function TQLogReader.First: Boolean;
begin
Result:=MoveTo(0);
end;

function TQLogReader.GetCount: Int64;
begin
Result:=0;
end;

function TQLogReader.Last: Boolean;
begin
Result:=MoveTo(Count-1);
end;

function TQLogReader.MoveTo(AIndex: Int64): Boolean;
begin
Result:=False;
end;

function TQLogReader.Next: Boolean;
begin
Result:=MoveTo(FItemIndex+1);
end;

function TQLogReader.Prior: Boolean;
begin
Result:=MoveTo(FItemIndex-1);
end;

function TQLogReader.ReadItem(var AMsg: QStringW;
  ALevel: TQLogLevel): Boolean;
begin
Result:=False;
end;

procedure TQLogReader.SetItemIndex(const Value: Int64);
begin
if FItemIndex<>Value then
  begin
  if not MoveTo(Value) then
    raise EXCEPTIOn.Create(Format(SLogSeekError,[Value]));
  end;
end;

//TQLogWriter
constructor TQLogWriter.Create;
begin
inherited;
end;

destructor TQLogWriter.Destroy;
begin
  inherited;
end;

procedure TQLogWriter.HandleNeeded;
begin
raise Exception.Create(SHandleNeeded);
end;

function TQLogWriter.WriteItem(AItem:PQLogItem): Boolean;
begin
Result:=False;
end;

{ TQLogFile }

constructor TQLogFileWriter.Create(const AFileName: QStringW; AWithIndex: Boolean);
begin
inherited Create;
FFileName:=AFileName;
FIndexNeeded:=AWithIndex;
FLogHandle:=nil;
FIndexHandle:=nil;
SetLength(FBuffer,65536);//64K缓冲区
FBuilder:=TQStringCatHelperW.Create;
end;

procedure TQLogFileWriter.CompressLog(ALogFileName, AIndexFileName: QStringW);
begin
{$IF RTLVersion<26}
if not Assigned(gzopen) then
  PostLog(llWarning,SZLibDLLMissed)
else
{$IFEND <XE5}
  TQLogCompressThread.Create(ALogFileName,AIndexFileName);
end;

constructor TQLogFileWriter.Create;
{$IFDEF MSWINDOWS}
var
  AExt:QStringW;
{$ENDIF MSWINDOWS}
begin
inherited Create;
FBuilder:=TQStringCatHelperW.Create;
{$IFDEF MSWINDOWS}
SetLength(FFileName,MAX_PATH);
{$IFDEF UNICODE}
SetLength(FFileName,GetModuleFileName(0,PQCharW(FFileName),MAX_PATH));
{$ELSE}
SetLength(FFileName,GetModuleFileNameW(0,PQCharW(FFileName),MAX_PATH));
{$ENDIF}
AExt:=ExtractFileExt(FFileName);
if Length(AExt)>0 then
  FFileName:=Copy(PQCharW(FFileName),0,Length(FFileName)-Length(AExt))+'.log'
else
  FFileName:=FFileName+'.log';
{$ELSE}
  FFileName:=TPath.GetSharedDocumentsPath+TPath.DirectorySeparatorChar+FormatDateTime('yyyymmddhhnnss',Now)+'.log';
{$ENDIF}
end;

destructor TQLogFileWriter.Destroy;
begin
FreeObject(FBuilder);
{$IFDEF NEXTGEN}
if Assigned(FLogHandle) then
  FLogHandle.DisposeOf;
if Assigned(FIndexHandle) then
  FIndexHandle.DisposeOf;
{$ELSE}
if Assigned(FLogHandle) then
  FLogHandle.Free;
if Assigned(FIndexHandle) then
  FIndexHandle.Free;
{$ENDIF UNICODE}
inherited;
end;

function TQLogFileWriter.FlushBuffer(AStream: TStream; p: Pointer;
  l: Integer): Boolean;
var
  AWriteBytes:Cardinal;
  ps:PByte;
begin
Result:=True;
ps:=p;
repeat
  AWriteBytes:=AStream.Write(ps^,l);
  if AWriteBytes=0 then
    begin
    FCastor.SetLastError(ELOG_WRITE_FAILURE,SysErrorMessage(GetLastError));
    Result:=False;
    Break
    end
  else
    begin
    Dec(l,AWriteBytes);
    Inc(ps,AWriteBytes);
    end;
until l=0;
end;

procedure TQLogFileWriter.HandleNeeded;
var
  AIndexFile,ALogFileName,AExt:QStringW;
  AIndex:Cardinal;
begin
if not Assigned(FLogHandle) then
  begin
  AIndex:=1;
  AExt:=ExtractFileExt(FFileName);
  ALogFileName:=Copy(FFileName,1,Length(FFileName)-Length(AExt));
  repeat
    try
      FLogHandle:=TFileStream.Create(FFileName,fmCreate);//好吧，创建的禁止他人写，我创建再打开还不行嘛
      FreeObject(FLogHandle);
      FLogHandle:=TFileStream.Create(FFileName,fmOpenWrite or fmShareDenyWrite);
    except
      FFileName:=ALogFileName+'_'+IntToStr(GetCurrentProcessId) +IntToStr(AIndex)+AExt;
      Inc(AIndex);
    end;
  until Assigned(FLogHandle) or (AIndex=100);
  if AIndex<>1 then
    AIndexFile:=ALogFileName+'_'+IntToStr(AIndex-1)+'.lidx'
  else
    AIndexFile:=ALogFileName+'.lidx';
  if FIndexNeeded then
    begin
    AIndex:=1;
    repeat
      try
        FIndexHandle:=TFileStream.Create(AIndexFile,fmCreate or fmShareDenyWrite);
      except
        AIndexFile:=ALogFileName+'_'+IntToStr(AIndex)+'.lidx';
        Inc(AIndex);
      end;
    until Assigned(FIndexHandle) or (AIndex=100);
    end;
  FLogHandle.WriteBuffer(#$FF#$FE,2);
  FPosition:=FLogHandle.Position;
  end;
end;

function TQLogFileWriter.WriteItem(AItem:PQLogItem): Boolean;
  procedure TimeChangeCheck;
  var
    ADayChanged:Boolean;
  begin
  if FLastTime<>AItem.TimeStamp then
    begin
    if Trunc(FLastTime)<>Trunc(AItem.TimeStamp) then
      ADayChanged:=True
    else
      ADayChanged:=False;
    FLastTime:=AItem.TimeStamp;
    if ADayChanged then
      FBuilder.Cat(FormatDateTime('[yyyy-mm-dd]',FLastTime)).Cat(SLineBreak);
    FLastTimeStamp:=FormatDateTime('[hh:nn:ss.zzz]',FLastTime);
    end;
  end;
  procedure RenameHistory;
  var
    ALogFileName,AOldName,AIndexFileName,ATimeStamp,AExt:QStringW;
  begin
  AOldName:=FLogHandle.FileName;
  FreeObject(FLogHandle);
  FLogHandle:=nil;
  AExt:=ExtractFileExt(AOldName);
  ATimeStamp:=FormatDateTime('yyyymmddhhnnsszzz',Now);
  ALogFileName:=StrDupX(PQCharW(AOldName),Length(AOldName)-Length(AExt))+'_'+
    ATimeStamp+AExt;
  RenameFile(FFileName,ALogFileName);
  if Assigned(FIndexHandle) then
    begin
    AOldName:=FIndexHandle.FileName;
    AExt:=ExtractFileExt(AOldName);
    FreeObject(FIndexHandle);
    AIndexFileName:=StrDupX(PQCharW(AOldName),Length(AOldName)-Length(AExt))+'_'+
      ATimeStamp+AExt;
    RenameFile(AOldName,AIndexFileName);
    FIndexHandle:=nil;
    end;
  HandleNeeded();
  //创建线程压缩日志文件
  CompressLog(ALogFileName,AIndexFileName);
  end;
begin
Result:=False;
if FLogHandle<>nil then
  begin
  FBuilder.Position:=0;
  TimeChangeCheck;
  if FLastThreadId<>AItem.ThreadId then
    begin
    FLastThreadId:=AItem.ThreadId;
    FLastThread:='['+IntToStr(FLastThreadId)+']';
    end;
  FBuilder.Cat(FLastTimeStamp).Cat(FLastThread).Cat(LogLevelText[AItem.Level]).Cat(':').Cat(@AItem.Text[0],AItem.MsgLen shr 1);
  if FIndexHandle=nil then
    FBuilder.Cat(SLineBreak);
  Result:=FlushBuffer(FLogHandle,FBuilder.Start,FBuilder.Position shl 1);
  if Result and (FIndexHandle<>nil) then
    Result:=FlushBuffer(FIndexHandle,@FPosition,SizeOf(Int64));
  Inc(FPosition,(FBuilder.Position shl 1));
  if (MaxLogFileSize>0) and (FPosition>=MaxLogFileSize) then //超过单个日志文件大小限制，将现在的日志文件重命名并压缩保存
    RenameHistory;
  end;
end;

{ TQLogCastor }
//后添加的始终在前面
procedure TQLogCastor.AddWriter(AWriter: TQLogWriter);
var
  AItem:PLogWriterItem;
begin
AWriter.HandleNeeded;
New(AItem);
AItem.Prior:=nil;
AItem.Writer:=AWriter;
AWriter.FCastor:=Self;
FCS.Enter;
AItem.Next:=FWriters;
if Assigned(FWriters) then
  FWriters.Prior:=AItem;
FWriters:=AItem;
FCS.Leave;
end;

constructor TQLogCastor.Create(AOwner:TQLog);
begin
inherited Create(true);
FCS:=TCriticalSection.Create;
FNotifyHandle:=TEvent.Create(nil,false,false,'');
FOwner:=AOwner;
Suspended:=False;
end;

destructor TQLogCastor.Destroy;
var
  AItem:PLogWriterItem;
begin
while Assigned(FWriters) do
  begin
  AItem:=FWriters.Next;
  FWriters.Writer.Free;
  Dispose(FWriters);
  FWriters:=AItem;
  end;
{$IFDEF NEXTGEN}
  FNotifyHandle.DisposeOf;
  FCS.DisposeOf;
{$ELSE}
  FNotifyHandle.Free;
  FCS.Free;
{$ENDIF}
inherited;
end;

procedure TQLogCastor.Execute;
var
  APrior:PQLogItem;
  procedure WriteItem;
  begin
  FirstWriter;
  while Assigned(FActiveWriter) do
    begin
    if not FActiveWriter.Writer.WriteItem(ActiveLog) then
      begin
      if FLastError=ELOG_WRITE_FAILURE then
        begin
        //Write Error handle
        end;
      end;
    NextWriter;
    end;
  end;
begin
while not Terminated do
  begin
  if WaitForLog then
    begin
    FActiveLog:=FetchNext;
    //开始写入日志
    while FActiveLog<>nil do
      begin
      Inc(FOwner.FFlushed);
      WriteItem;
      APrior:=FActiveLog;
      FActiveLog:=APrior.Next;
      FreeItem(APrior);
      end;
    end;
  end;
end;

function TQLogCastor.FetchNext: PQLogItem;
begin
Result:=FOwner.Pop;
end;

function TQLogCastor.FirstWriter: PLogWriterItem;
begin
FActiveWriter:=FWriters;
Result:=FActiveWriter;
end;

{$IFNDEF UNICODE}
function TQLogCastor.GetFinished: Boolean;
begin
//Delphi 2007
{$IF RTLVersion=18.5}
Result:=PBoolean(Integer(Self)+16)^;
{$ELSE}
raise Exception.Create('未受支持的版本，需要检查TThread.FFinished的偏移后再加入支持。');
{$IFEND}
end;
{$ENDIF}

procedure TQLogCastor.LogAdded;
begin
FNotifyHandle.SetEvent;
end;

function TQLogCastor.NextWriter: PLogWriterItem;
begin
FCS.Enter;
if Assigned(FActiveWriter) then
  FActiveWriter:=FActiveWriter.Next;
Result:=FActiveWriter;
FCS.Leave;
end;

procedure TQLogCastor.RemoveWriter(AWriter: TQLogWriter);
var
  AItem:PLogWriterItem;
begin
AItem:=nil;
repeat
  FCS.Enter;
  if not Assigned(FActiveWriter) or (FActiveWriter.Writer<>AWriter) then
    begin
    AItem:=FWriters;
    while Assigned(AItem) do
      begin
      if AItem.Writer=AWriter then
        begin
        if Assigned(AItem.Prior) then
          AItem.Prior.Next:=AItem.Next;
        if Assigned(AItem.Next) then
          AItem.Next.Prior:=AItem.Prior;
        Break;
        end;
      end;
    FCS.Leave;
    Break;
    end
  else
    begin
    FCS.Leave;
    Yield;//将执行时间片交给其它线程
    end;
until 1>2;
if Assigned(AItem) then
  Dispose(AItem);
end;

procedure TQLogCastor.SetLastError(ACode: Cardinal; const AMsg: QStringW);
begin
FLastError:=ACode;
FLastErrorMsg:=AMsg;
end;

function TQLogCastor.WaitForLog: Boolean;
begin
Result:=(FNotifyHandle.WaitFor(INFINITE)=wrSignaled);
end;

{ TQLog }

function TQLog.Pop: PQLogItem;
begin
Lock;
Result:=FList.First;
FList.Value:=0;
Unlock;
end;

procedure TQLog.Post(ALevel: TQLogLevel; const AFormat: QStringW;
  Args: array of const);
begin
{$IFDEF NEXTGEN}
Logs.Post(ALevel,Format(AFormat,Args));
{$ELSE}
Logs.Post(ALevel,WideFormat(AFormat,Args));
{$ENDIF}
end;

procedure TQLog.Post(ALevel: TQLogLevel;const AMsg: QStringW);
var
  AItem:PQLogItem;
begin
if FInFree then
  Exit;
AItem:=CreateItem(AMsg,ALevel);
AItem.Next:=nil;
//使用临界锁定版
Lock();
Inc(FCount);
if FList.FirstVal=0 then
  begin
  FList.First:=AItem;
  FList.Last:=AItem;
  end
else
  begin
  FList.Last.Next:=AItem;
  FList.Last:=AItem;
  end;
Unlock;
FCastor.LogAdded;
end;

constructor TQLog.Create;
begin
inherited;
FList.Value:=0;
FInFree:=False;
FCastor:=TQLogCastor.Create(Self);
FCS:=TCriticalSection.Create;
end;

function TQLog.CreateCastor: TQLogCastor;
begin
Result:=TQLogCastor.Create(Self);
end;

destructor TQLog.Destroy;
begin
FInFree:=True;
//等待日志全部写入完成
while Assigned(FList.First) do
  Sleep(10);
FCastor.Terminate;
FCastor.FNotifyHandle.SetEvent;
while not FCastor.Finished do
  Sleep(10);
FreeObject(FCastor);
FreeObject(FCS);
inherited;
end;

function TQLog.GetCastor: TQLogCastor;
begin
if FCastor=nil then
  FCastor:=CreateCastor;
Result:=FCastor;
end;

procedure TQLog.Lock;
begin
FCS.Enter;
end;

procedure TQLog.Unlock;
begin
FCS.Leave;
end;
{ TQLogConsoleWriter }

constructor TQLogConsoleWriter.Create;
begin
inherited;
end;

procedure TQLogConsoleWriter.HandleNeeded;
begin
//Nothing Needed
end;

function TQLogConsoleWriter.WriteItem(AItem:PQLogItem): Boolean;
{$IFDEF MSWINDOWS}
var
  S:QStringW;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
S:=FormatDateTime('hh:nn:ss.zzz',AItem.TimeStamp)+' ['+IntToStr(AItem.ThreadId)+'] '+
  StrDupX(@AItem.Text[0],AItem.MsgLen shr 1);
OutputDebugStringW(PWideChar(S));
Result:=True;
{$ELSE}
//Posix不支持写日志到控制台
Result:=False;
{$ENDIF}
end;

{ TQLogSocketWriter }

constructor TQLogSocketWriter.Create;
begin
inherited;
FServerPort:=514;//Syslog端口
FTextEncoding:=teAnsi;
FBuilder:=TQStringCatHelperW.Create(1024);//Syslog默认不超过1024字节
end;

destructor TQLogSocketWriter.Destroy;
begin
FreeObject(FBuilder);
if FSocket<>THandle(-1) then
  begin
  {$IFDEF MSWINDOWS}
  closesocket(FSocket);
  WSACleanup;
  {$ELSE}
  __close(FSocket);
  {$ENDIF}
  end;
inherited;
end;

procedure TQLogSocketWriter.HandleNeeded;
{$IFDEF MSWINDOWS}
var
  AData:WSAData;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
if WSAStartup(MakeWord(1,1),AData)<>0 then
  RaiseLastOSError(WSAGetLastError);
{$ENDIF}
FSocket:=socket(AF_INET,SOCK_DGRAM,17);
if FSocket=THandle(-1) then
  RaiseLastOSError;
FReaderAddr.sin_family:=AF_INET;
FReaderAddr.sin_port:=htons(ServerPort);
FReaderAddr.sin_addr.s_addr:=inet_addr(Pointer(PQCharA(QString.AnsiEncode(ServerHost))));
PInt64(@FReaderAddr.sin_zero[0])^:=0;
end;

procedure TQLogSocketWriter.SetTextEncoding(const Value: TTextEncoding);
begin
if Value in [teAnsi,teUtf8] then
  begin
  if FTextEncoding <> Value then
    FTextEncoding:=Value;
  end
else
  raise Exception(SUnsupportSysLogEncoding);
end;

function TQLogSocketWriter.WriteItem(AItem:PQLogItem): Boolean;
var
  p:PQCharA;
  ASize,ALen:Integer;
  APri:QStringW;
  AHeader,AText:QStringA;
  ABuf:array[0..1023] of Byte;
  procedure CalcPri;
  begin
  APri:='<'+IntToStr(8+Integer(FCastor.ActiveLog.Level))+'>';
  {
   0       Emergency: system is unusable
   1       Alert: action must be taken immediately
   2       Critical: critical conditions
   3       Error: error conditions
   4       Warning: warning conditions
   5       Notice: normal but significant condition
   6       Informational: informational messages
   7       Debug: debug-level messages
  }
  end;

  function FormatSyslogTime:QStringW;
  var
    Y,M,D:Word;
  const
    LinuxMonth:array[0..11] of QStringW=(
      'Jan', 'Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
      'Oct', 'Nov', 'Dec');
  begin
  DecodeDate(AItem.TimeStamp,Y,M,D);
  //Aug 24 05:34:00 CST 1987
  Result:=LinuxMonth[M-1];
  if D<10 then
    Result:=Result+'  '+IntToStr(D)
  else
    Result:=Result+' '+IntToStr(D);
  Result:=Result+' '+FormatDateTime('hh:nn:ss',AItem.TimeStamp)+' ';
  end;

  function HostName:QStringW;
  var
    AName:QStringA;
  begin
  AName.Length:=64;
  gethostname(Pointer(PQCharA(AName)),64);
  Result:=DeleteCharW(QString.AnsiDecode(PQCharA(AName),-1),' '#9#10#13)+' ';
  end;
  function CopyText:Integer;
  var
    ps,pd:PQCharA;
    ACharSize:Integer;
  begin
  if ALen+AHeader.Length<1024 then
    begin
    Move(p^,ABuf[AHeader.Length],ALen);
    Inc(p,ALen);
    Result:=AHeader.Length+ALen;
    ALen:=0;
    end
  else
    begin
    pd:=@ABuf[AHeader.Length];
    ps:=@ABuf[0];
    Result:=AHeader.Length;
    while p^<>0 do
      begin
      if TextEncoding=teAnsi then
        ACharSize:=CharSizeA(p)
      else
        ACharSize:=CharSizeU(p);
      if (IntPtr(pd)-IntPtr(ps))+ACharSize<=1024 then
        begin
        while ACharSize>0 do
          begin
          pd^:=p^;
          Inc(p);
          Inc(pd);
          Dec(ACharSize);
          end;
        end
      else
        begin
        Result:=IntPtr(pd)-IntPtr(ps);
        Dec(ALen,Result-AHeader.Length);
        Break;
        end;
      end;
    end;
  end;
begin
Result:=True;
FBuilder.Position:=0;
CalcPri;
FBuilder.Cat(APri);
FBuilder.Cat(FormatSyslogTime);
FBuilder.Cat(HostName);
FBuilder.Cat('[').Cat(IntToStr(AItem.ThreadId)).Cat(']');
FBuilder.Cat(LogLevelText[AItem.Level]);
AHeader:=QString.Utf8Encode(FBuilder.Value);
if TextEncoding=teAnsi then
  AText:=QString.AnsiEncode(@AItem.Text[0],AItem.MsgLen shr 1)
else
  AText:=QString.Utf8Encode(@AItem.Text[0],AItem.MsgLen shr 1);
p:=PQCharA(AText);
ALen:=AText.Length;
repeat
  Move(PQCharA(AHeader)^,ABuf[0],AHeader.Length);
  ASize:=CopyText;
  sendto(FSocket,ABuf[0],ASize,0,PSockAddr(@FReaderAddr)^,sizeof(sockaddr_in));
until ALen<=0;
end;

{ TQLogCompressThread }

constructor TQLogCompressThread.Create(ALogFileName, AIndexFileName: QStringW);
begin
FLogFileName:=ALogFileName;
FIndexFileName:=AIndexFileName;
inherited Create(True);
FreeOnTerminate:=True;
Suspended:=False;
end;

procedure TQLogCompressThread.Execute;
const
  {$IFDEF NEXTGEN}
  AMode:MarshaledAString='wb';
  {$ELSE}
  AMode:PAnsiChar='wb';
  {$ENDIF}
  procedure DoCompress(AFileName:QStringW);
  var
    AFile:gzFile;
    ABuf:array[0..65535] of Byte;
    AStream:TFileStream;
    AReaded:Integer;
  begin
  AStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    AFile:=gzOpen(Pointer(PQCharA(AnsiEncode(AFileName+'.gz'))),AMode);
    if AFile<>nil then
      begin
      repeat
        AReaded:=AStream.Read(ABuf[0],65536);
        if AReaded>0 then
          gzwrite(AFile,Abuf[0],AReaded);
      until AReaded=0;
      gzclose(AFile);
      end;
  finally
    AStream.Free;
    Sysutils.DeleteFile(AFileName);
  end;
  end;
begin
if Length(FLogFileName)>0 then
  DoCompress(FLogFileName);
if Length(FIndexFileName)>0 then
  DoCompress(FIndexFileName);
end;

initialization
  MaxLogFileSize:=-1;
  {$IFDEF QLOG_CREATE_GLOBAL}
  Logs:=TQLog.Create;
  {$IFDEF QLOG_CREATE_DEFAULT_WRITER}
  Logs.Castor.AddWriter(TQLogFileWriter.Create);
  {$ENDIF}
  {$ELSE}
  Logs:=nil;
  {$ENDIF}
  {$IF RTLVersion<26}
  zlibhandle:=LoadLibrary('zlib1.dll');
  if zlibhandle<>0 then
    begin
    gzopen:=GetProcAddress(zlibhandle,'gzopen');
    gzseek:=GetProcAddress(zlibhandle,'gzseek');
    gztell:=GetProcAddress(zlibhandle,'gztell');
    gzwrite:=GetProcAddress(zlibhandle,'gzwrite');
    gzclose:=GetProcAddress(zlibhandle,'gzclose');
    end
  else
    begin
    gzopen:=nil;
    gzseek:=nil;
    gztell:=nil;
    gzwrite:=nil;
    gzclose:=nil;
    end;
  {$IFEND <XE5}
finalization
  {$IFDEF QLOG_CREATE_GLOBAL}
  FreeObject(Logs);
  Logs:=nil;
  {$ENDIF}
  {$IF RTLVersion<26}
  if zlibhandle<>0 then
    FreeLibrary(zlibhandle);
  {$IFEND <XE5}
end.
