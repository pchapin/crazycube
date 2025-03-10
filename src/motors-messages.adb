--------------------------------------------------------------------------------
-- FILE   : motors-messages.adb
-- SUBJECT: Body of a package that implements the main part of the motors module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with motors.API;  -- Needed so that the types in the API can be used here.
with CubedOS.Log_Server.API;

package body motors.Messages is
   use Message_Manager;
   MotorOne : Float := 0;
   MotorTwo : Float := 0;
   MotorThree : Float := 0;
   MotorFour : Float := 0;
   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Increase_Voltage_Request(Message : in Message_Record)
     with Pre => Sample_Module.API.Is_A_Request(Message)
   is
      Status  : Message_Status_Type;
      VoltageOne : Float;
      VoltageTwo : Float;
      VoltageThree : Float;
      VoltageFour: Float;
      Move_Reply : Message_Record;
   begin
      Motors.API.Increase_Voltage_Decode
        (Message       => Message,
         VoltageOne    => VoltageOne,
         VoltageTwo    => VoltageTwo,
         VoltageThree  => VoltageThree,
         VoltageFour   => VoltageFour,
         Decode_Status => Message_Status_Type);
      -- moves the drone
      MotorOne := MotorOne + VoltageOne;
      MotorTwo := MotorTwo + VoltageTwo;
      MotorThree := MotorThree + VoltageThree;
      MotorOne := MotorOne + VoltageFour;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID       => 1,
         Success          => True,
         Priority         => System.Priority);
      Route_Message (Message => Move_Reply);
   end Handle_Increase_Voltage_Request;

   procedure Handle_Decrease_Voltage_Request(Message : in Message_Record)
     with Pre => Sample_Module.API.Is_A_Request(Message)
   is
      Status  : Message_Status_Type;
      VoltageOne : Float;
      VoltageTwo : Float;
      VoltageThree : Float;
      VoltageFour: Float;
      Move_Reply : Message_Record;
   begin
      Motors.API.Decrease_Voltage_Decode
        (Message       => Message,
         VoltageOne    => VoltageOne,
         VoltageTwo    => VoltageTwo,
         VoltageThree  => VoltageThree,
         VoltageFour   => VoltageFour,
         Decode_Status => Message_Status_Type);
      -- moves the drone
      MotorOne := MotorOne - VoltageOne;
      MotorTwo := MotorTwo - VoltageTwo;
      MotorThree := MotorThree - VoltageThree;
      MotorOne := MotorOne - VoltageFour;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID       => 1,
         Success          => True,
         Priority         => System.Priority);
      Route_Message (Message => Move_Reply);
   end Handle_Decrease_Voltage_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Motors.API.Is_Move_Request(Message => Message_Record) then
         Handle_Move_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Motors,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Motors.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Motors.Messages;
