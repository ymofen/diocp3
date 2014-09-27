(*	
 *	 Unit owner: d10.ÃÏµÿœ“
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v0.1(2014-7-16 21:36:30)
 *     + first release
 *) 
unit iocpProtocol;

interface

uses
  Windows, SysUtils;


{$if CompilerVersion < 23}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}

// before delphi 2007
{$if CompilerVersion < 18}
type
     ULONG_PTR = Cardinal;
{$ifend}



const
  IOCP_RESULT_OK = 0;
  IOCP_RESULT_QUIT = 1;


function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey:ULONG_PTR; NumberOfConcurrentThreads: DWORD): THandle; stdcall;
{$EXTERNALSYM CreateIoCompletionPort}

function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred, lpCompletionKey: ULONG_PTR;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetQueuedCompletionStatus}


implementation

function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';
function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';

end.
