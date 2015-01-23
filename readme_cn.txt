设置环境变量
DIOCP3_HOME=E:\workspace\diocp3

搜索路径
$(DIOCP3_HOME)\Source

编码器和解码器路径,如果使用DEMO中的解码器和编码器，可以加入到搜索路径
$(DIOCP3_HOME)\samples\socket-Coder\diocpCoders

目录说明:
  samples                        下面是各种DEMO
  samples\socket-Coder           是编码器的使用DEMO
  source                         目录下面是源代码
  
可以先从下面的DEMO中了解diocp的工作原理
  samples\ECHO
  samples\simple 


=======================================================
关于文档帮助

关于DIOCP3的说明书<陆续补充>:
http://www.cnblogs.com/DKSoft/category/610350.html

DIOCP的点点滴滴：
http://www.cnblogs.com/DKSoft/category/471197.html

DIOCP QQ 群: 320641073  
DIOCP官方社区: www.diocp.org
=======================================================

关于捐助：

	DIOCP3遵循BSD协议，你可以任意的用于商业项目和自由的项目中而不用通知我，
	如果你觉得DIOCP3对你有帮助而你刚好又想对DIOCP3进行捐助,请联系作者，或者直接进行捐助：

	捐助的支付宝：
	账号：ymofen@diocp.org
	户名: 杨茂丰
	
 


=======================================================


重写了DIOCP，版本从3开始。

新特性:
  1.重新规划了整个设计。抽象IOCP层->IOCP-Socket(Socket应用),将iocp独立出来，可以做更多的iocp应用。
  2.改用投递Request->Response的模式，这样每个投递的请求，都可以处理响应。(感谢ryan提供的思路)
  3.去掉了IO内存池，服务端可以0拷贝处理和转移数据。
  4.去掉了Accept线程，改用AcceptEx,可以瞬间响应连接更多的连接。
  5.添加了TIocpTcpClient客户端组件，iocp客户端的应用，可以很好的做推送消息客户端。
  6.组件化TIocpTcpClient和TIocpTcpServer。

  7.添加了基于IOCP引擎的任务投递(异步+回调)库iocp-task
  8.封装了DIOCP1函数接口类，同样支持解码和编码器。

感谢qsl给提供的很多建议。
