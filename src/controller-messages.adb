--------------------------------------------------------------------------------
-- FILE   : Controller-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;    -- See the comments in SAMPLE_MODULE-api.ads.
with Name_Resolver;      -- See the comments in SAMPLE_MODULE-api.ads.
with CubedOS.Log_Server.API;

package body Controller.Messages is
   use Message_Manager;

   -- The package initializer, if needed. This procedure might be called as the message loop
   -- (see below) is starting, or perhaps during package elaboration. If this procedure is not
   -- needed, it should be removed to avoid SPARK flow warnings.
   --
   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   -- Here is where the details of handing the messages is done. Probably there will be a
   -- separate subprogram for each message, but the exact organization is, obviously, dependent
   -- on the needs of the module. It might be useful to put these message handling subprograms
   -- into a private sibling package. That would move the complex details of message handling to
   -- a different file and reduce clutter in this file. This file is really just about decoding
   -- and dispatching the messages. We recommend that if a single internal package is used that
   -- it should be called Sample_Module.Core (for example).

   procedure Handle_A_Request(Message : in Message_Record)
    -- with Pre => Sample_Module.API.Is_A_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      -- Sample_Module.API.A_Request_Decode(Message, Status);
      null;
      -- Act on the request message.
   end Handle_A_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Sample_Module.API.Is_A_Request(Message) then
         Handle_A_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Controller,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;
      -- When this procedure returns the message loop will immediately try to receive the next
      -- message. Note that all CubedOS send operations are non-blocking so sending an outgoing
      -- message will not delay execution.
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      Initialize;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Controller.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Controller.Messages;
