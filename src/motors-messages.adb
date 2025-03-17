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
   Motor_One : Float := 0.0;
   Motor_Two : Float := 0.0;
   Motor_Three : Float := 0.0;
   Motor_Four : Float := 0.0;
   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Increase_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Increase_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Float;
      Voltage_Two : Float;
      Voltage_Three : Float;
      Voltage_Four: Float;
      Move_Reply : Message_Record;
   begin
      Motors.API.Increase_Voltage_Decode
        (Message       => Message,
         VoltageOne    => Voltage_One,
         VoltageTwo    => Voltage_Two,
         VoltageThree  => Voltage_Three,
         VoltageFour   => Voltage_Four,
         Decode_Status => Status);
      -- moves the drone
      Motor_One := Motor_One + Voltage_One;
      Motor_Two := Motor_Two + Voltage_Two;
      Motor_Three := Motor_Three + Voltage_Three;
      Motor_Four := Motor_Four + Voltage_Four;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID       => 1,
         Successful          => True);
      Route_Message (Message => Move_Reply);
   end Handle_Increase_Voltage_Request;

   procedure Handle_Decrease_Voltage_Request(Message : in Message_Record)
     with Pre => Motors.API.Is_Decrease_Voltage(Message)
   is
      Status  : Message_Status_Type;
      Voltage_One : Float;
      Voltage_Two : Float;
      Voltage_Three : Float;
      Voltage_Four: Float;
      Move_Reply : Message_Record;
   begin
      Motors.API.Decrease_Voltage_Decode
        (Message       => Message,
         VoltageOne    => Voltage_One,
         VoltageTwo    => Voltage_Two,
         VoltageThree  => Voltage_Three,
         VoltageFour   => Voltage_Four,
         Decode_Status => Status);
      -- moves the drone
      Motor_One := Motor_One - Voltage_One;
      Motor_Two := Motor_Two - Voltage_Two;
      Motor_Three := Motor_Three - Voltage_Three;
      Motor_Four := Motor_Four - Voltage_Four;

      Move_Reply := Motors.API.Move_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID       => 1,
         Successful          => True);
      Route_Message (Message => Move_Reply);
   end Handle_Decrease_Voltage_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Motors.API.Is_Increase_Voltage(Message => Message) then
         Handle_Increase_Voltage_Request(Message);
      elsif Motors.API.Is_Decrease_Voltage(Message => Message) then
         Handle_Decrease_Voltage_Request(Message);
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
