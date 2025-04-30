--------------------------------------------------------------------------------
-- FILE   : sensor-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2025 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Name_Resolver;
with Sensors.API; use Sensors.API;
with CubedOS.Log_Server.API;

package body Sensors.Messages is
   use Message_Manager;
   Altitude : State_Type := 0; -- keeps track of the dumy altitude at all time

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Get_Dumy_Altitude_Request(Message: in Message_Record)
     with Pre => Sensors.API.Is_Get_Dumy_Altitude_Request(Message)
   is
      Altitude_Reply : Message_Record;
   begin
      Altitude_Reply := Sensors.API.Get_Dumy_Altitude_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID => 1,
         Inches => Altitude);
      Route_Message(Altitude_Reply);
   end Handle_Get_Dumy_Altitude_Request;

   procedure Handle_Increase_Dumy_Altitude_Request(Message : in Message_Record)
     with Pre => Sensors.API.Is_Increase_Dumy_Altitude_Request(Message)
   is
      Increase_Reply : Message_Record;
      Inches : State_Type;
      Status : Message_Status_Type;
      Successful : Sensors.API.Status_Type := Success;
   begin
      Sensors.API.Increase_Dumy_Altitude_Request_Decode
        (Message => Message,
         Inches => Inches,
         Decode_Status => Status);
      if Altitude + Inches > 100 then
         Successful := Failure;
      else
         Altitude := Altitude + Inches;
      end if;

      Increase_Reply := sensors.API.Increase_Dumy_Altitude_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID => 1,
         Successful => Successful);
      Route_Message(Message => Increase_Reply);
   end Handle_Increase_Dumy_Altitude_Request;

   procedure Handle_Decrease_Dumy_Altitude_Request(Message : in Message_Record)
     with Pre => Sensors.API.Is_Decrease_Dumy_Altitude_Request(Message)
   is
      Decrease_Reply : Message_Record;
      Inches : State_Type;
      Status : Message_Status_Type;
      Successful : Sensors.API.Status_Type := Success;
   begin
      Sensors.API.Decrease_Dumy_Altitude_Request_Decode
        (Message => Message,
         Inches => Inches,
         Decode_Status => Status);

      if Altitude - Inches < 0 then
         Successful := Failure;
      else
         Altitude := Altitude - Inches;
      end if;

      Decrease_Reply := sensors.API.Decrease_Dumy_Altitude_Reply_Encode
        (Receiver_Address => Message.Sender_Address,
         Request_ID => 1,
         Successful => Successful);
      Route_Message(Message => Decrease_Reply);
   end Handle_Decrease_Dumy_Altitude_Request;

   -- returns 0 for all because of the simulation
   procedure Handle_Get_Gyro_Measurements_Request(Message : in Message_Record)
     with Pre => Sensors.API.Is_Get_Gyro_Measurements_Request(Message)
   is
      Measurments_Reply : Message_Record;
   begin
      Measurments_Reply := Sensors.API.Get_Gyro_Measurements_Reply_Encode
        (Message.Sender_Address,
         1,
         Success,
         0,
         0,
         0);
      Route_Message(Measurments_Reply);
   end Handle_Get_Gyro_Measurements_Request;

   -- returns 0 for all because of the simulation
   procedure Handle_Get_Accel_Measurements_Request(Message : in Message_Record)
     with Pre => Sensors.API.Is_Get_Accel_Measurements_Request(Message)
   is
      Measurments_Reply : Message_Record;
   begin
      Measurments_Reply := Sensors.API.Get_Accel_Measurements_Reply_Encode
        (Message.Sender_Address,
         1,
         Success,
         0,
         0,
         0);
      Route_Message(Measurments_Reply);
   end Handle_Get_Accel_Measurements_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process(Message : in Message_Record) is
   begin
      if Sensors.API.Is_Get_Gyro_Measurements_Request(Message) then
         Handle_Get_Gyro_Measurements_Request(Message);
      elsif Sensors.API.Is_Get_Accel_Measurements_Request(Message) then
         Handle_Get_Accel_Measurements_Request(Message);
      elsif Sensors.API.Is_Decrease_Dumy_Altitude_Request(Message) then
         Handle_Decrease_Dumy_Altitude_Request(Message);
      elsif Sensors.API.Is_Increase_Dumy_Altitude_Request(Message) then
         Handle_Increase_Dumy_Altitude_Request(Message);
      elsif Sensors.API.Is_Get_Dumy_Altitude_Request(Message) then
         Handle_Get_Dumy_Altitude_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Sensors,
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
         Message_Manager.Fetch_Message(Name_Resolver.Sensors.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Sensors.Messages;
