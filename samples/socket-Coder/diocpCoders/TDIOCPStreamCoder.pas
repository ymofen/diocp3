unit TDIOCPStreamCoder;

interface

uses
  uIocpCoder, uBuffer, Classes, SysUtils;

type
  TIOCPStreamDecoder = class(TIOCPDecoder)
  public
    /// <summary>
    ///   解码收到的数据,如果有接收到数据,调用该方法,进行解码
    /// </summary>
    /// <returns>
    ///   返回解码好的对象
    /// </returns>
    /// <param name="inBuf"> 接收到的流数据 </param>
    function Decode(const inBuf: TBufferLink): TObject; override;
  end;


  TIOCPStreamEncoder = class(TIOCPEncoder)
  public
    /// <summary>
    ///   编码要发生的对象
    /// </summary>
    /// <param name="pvDataObject"> 要进行编码的对象 </param>
    /// <param name="ouBuf"> 编码好的数据 </param>
    procedure Encode(pvDataObject:TObject; const ouBuf: TBufferLink); override;
  end;

implementation

uses
  uByteTools;

const
  PACK_FLAG = $D10;

  //PACK_FLAG  + STREAM_LEN + STREAM_DATA

  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。



function TIOCPStreamDecoder.Decode(const inBuf: TBufferLink): TObject;
begin
  ;
end;

{ TIOCPStreamEncoder }

procedure TIOCPStreamEncoder.Encode(pvDataObject: TObject;
  const ouBuf: TBufferLink);
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
begin
  lvPACK_FLAG := PACK_FLAG;

  if TStream(pvDataObject).Size > MAX_OBJECT_SIZE then
  begin
    if lvDataLen > MAX_OBJECT_SIZE then
      raise Exception.CreateFmt('数据包太大,请在业务层分拆发送,最大数据包[%d]!', [MAX_OBJECT_SIZE]);
  end;

  //
  lvDataLen := MAX_OBJECT_SIZE;
  lvWriteIntValue := TByteTools.swap32(lvDataLen);

  //pack_flag
  ouBuf.AddBuffer(@lvPACK_FLAG, 2);

  // stream len
  ouBuf.AddBuffer(@lvWriteIntValue, SizeOf(lvWriteIntValue));

  SetLength(lvBuf, lvDataLen);
  TStream(pvDataObject).Read(lvBuf[0], lvDataLen);

  // stream
  ouBuf.AddBuffer(@lvBuf[0], lvDataLen);  
end;

end.
