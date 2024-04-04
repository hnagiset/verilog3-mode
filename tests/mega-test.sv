module foo (
input logic in,
output logic out,
input custom_t my_type,
input [3:0] byte_t my_word_array_packed,
output other_t other_type_array_unpacked [3],
input custom_e my_enum,
output custom_s my_struct,
custom_if my_if
);

custom_vif my_vif;
logic sig1;
logic sig2;
custom_t my_type1;
other_t other_type1;
custom_if my_if1;

endmodule

// Local Variables:
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_\\(t\\|e\\|s\\|if\\|vif\\)\\>"
// End:
// New File: <align_declarations.v>
module autoarg_comment
  
  // Copyright 1997-1998, blah, blah, blah

  (/*AUTOARG*/);

   //verilint 332 OFF	//* Not all possible cases covered, but default case exists

   input (* ATTRIB="val" *) 	income;
   output			outgo2;

endmodule
// New File: <autoarg_comment.v>
module tahoetop(/*AUTOARG*/);

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics


   // the pad instance to look like "pads pads (/*AUTOINST*/". Then at
   // diff the new against the old. Finally delete /*AUTOINST*/. This will

endmodule
// New File: <autoarg_jwells_comment.v>
// bug360
module f (/*AUTOARG*/);

always @* r = "test/*";

  input 				x;

endmodule
// New File: <autoarg_quote_cmt.v>
// msg1354
module f (/*AUTOARG*/);

   output o2;
   input z;
   input a;
   input q;
   output o1;
endmodule

// Local Variables:
// verilog-auto-arg-format:single
// End:
// New File: <autoarg_single.v>
module (/*AUTOARG*/);
   output o2;
   input z;
   input a;
   input q;
   output o1;
endmodule

// Local Variables:
// verilog-auto-arg-sort:t
// End:
// New File: <autoarg_sort_fedeli.v>
// bug #1829
module dut (/*autoarg*/);
input clk;
input rst_n;
output logic d;

always @(* )
begin
  d = 1'b0;
end

endmodule
// New File: <autoarg_starparen.v>
module bug_minimal
  (input wire GND,
   input wire VDD,

   inout wire PIN1,
   inout wire PIN2,
   inout wire PIN3,
   inout wire PIN4
   
   /*AUTOARG*/);

   // ----------------------------------------------------------------
   /*AUTOWIRE*/

   // ----------------------------------------------------------------
   task force_value_to_1;
      begin
         $display("Enable test module checking ...");
         force `value = 1'b1;
      end
   endtask

   // ---------------------------------------------------------------
   task force_value_to_0;
      begin
         $display("Disable test module checking ...");
         force `value = 1'b0;
      end
   endtask
   
endmodule
// New File: <autoarg_string_bug259.v>
module top
  (
   /*AUTOINPUT*/
   );

   /*AUTOWIRE*/

   inst_module inst (/*AUTOINST*/);
endmodule

module inst_module (input supply0 VDD,
                    input supply1 VSS,
		    input non_supply,
		    input supply1 VDD_from_child
		    );

endmodule
// New File: <autoarg_supply_bug438.v>
// Code ok to distribute

module autoasciienum_auto();

   reg [2:0]   /* auto enum sm_psm */   sm_psm;
   reg [2:0]   /* auto enum sm_ps2 */   sm_ps2;

   localparam [2:0] // auto enum sm_psm
     PSM_IDL = 0,
     PSM_RST = 6,
     PSM_ZOT = 7;

   localparam [2:0] // auto enum sm_ps2
     PS2_IDL = 0,
     PS2_FOO = 1;

   /*AUTOASCIIENUM("sm_psm", "_sm_psm__ascii", "_")*/
   /*AUTOASCIIENUM("sm_ps2", "_sm_ps2__ascii", "_")*/

endmodule : autoasciienum_auto


// New File: <autoasciienum_auto.v>
module sm (/*AUTOARG*/
   // Outputs
   state_e1, state_r
   );

   output [2:0] state_e1;
   output [2:0] state_r;

   //== State enumeration
   parameter [2:0] // synopsys enum state_info
		SM_IDLE =  3'b000,
		SM_ACT = 3'b010;
   //== State variables
   reg [2:0] 	   /* synopsys enum state_info */
		   state_r;		/* synopsys state_vector state_r */
   reg [2:0] 	   /* synopsys enum state_info */
		   state_e1;

   //== ASCII state decoding

   /*AUTOASCIIENUM("state_r", "_stateascii_r", "sm_")*/
   // Beginning of automatic ASCII enum decoding
   reg [31:0]		_stateascii_r;		// Decode of state_r
   always @(state_r) begin
      case ({state_r})
	SM_IDLE:  _stateascii_r = "idle";
	SM_ACT:   _stateascii_r = "act ";
	default:  _stateascii_r = "%Err";
      endcase
   end
   // End of automatics

endmodule
// New File: <autoasciienum_ex.v>
module sm (/*AUTOARG*/);

   //==================== Constant declarations ==============

`include "autoasciienum_param.v"

   //==================== Intermediate Variables =============

   reg [3:0]	/* synopsys enum En_C14ChipNum */	chip_r;

   //==================== DEBUG ASCII CODE =========================

   /*AUTOASCIIENUM("chip_r", "chip_r__ascii","Ep_C14ChipNum_")*/
   // Beginning of automatic ASCII enum decoding
   reg [31:0]		chip_r__ascii;		// Decode of chip_r
   always @(chip_r) begin
      case ({chip_r})
	EP_C14ChipNum_RNP:  chip_r__ascii = "rnp ";
	EP_C14ChipNum_SPP:  chip_r__ascii = "spp ";
	EP_C14ChipNum_SRP:  chip_r__ascii = "srp ";
	EP_C14ChipNum_SMM2: chip_r__ascii = "smm2";
	EP_C14ChipNum_SMM:  chip_r__ascii = "smm ";
	EP_C14ChipNum_TTE:  chip_r__ascii = "tte ";
	EP_C14ChipNum_DLE:  chip_r__ascii = "dle ";
	EP_C14ChipNum_OASP: chip_r__ascii = "oasp";
	default:            chip_r__ascii = "%Err";
      endcase
   end
   // End of automatics

endmodule

//==== Emacs verilog-mode controls ====
// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autoasciienum_frominc.v>
// lint_checking MODLNM OFF

module autoasciienum_onehot (
  input    clk,
  input    rst_n,
  output   ack
);

  localparam // synopsys enum state_info
                IDLE = 0,
                S1   = 1,
                S2   = 2,
                S3   = 3,
                DONE = 4;

  reg [4:0] // synopsys enum state_info
                cur_state, nxt_state;

  always @ (*) begin
    nxt_state = 5'h0;

    case (1'b1)
      cur_state[IDLE] : nxt_state[S1] = 1'b1;
      cur_state[S1]   : nxt_state[S2] = 1'b1;
      cur_state[S2]   : nxt_state[S3] = 1'b1;
      cur_state[S3]   : nxt_state[DONE] = 1'b1;
      cur_state[DONE] : nxt_state[DONE] = 1'b1;
    endcase
  end

  always @ (posedge clk or negedge rst_n)
    if (rst_n == 1'b0) begin
      cur_state <= 'h1;
    end
    else begin
      cur_state <= nxt_state;
    end

  assign ack = cur_state[DONE];

  /*AUTOASCIIENUM("cur_state", "cur_state_ascii")*/

endmodule
// New File: <autoasciienum_onehot.v>

`ifdef NOTDEFINED
module just_for_proper_indentation ();
`endif

   //Verilint 175 off //WARNING: Unused parameter

   parameter // synopsys enum En_C14ChipNum
     EP_C14ChipNum_OASP =               4'h0,
       EP_C14ChipNum_DLE =                4'h2,
       EP_C14ChipNum_TTE =                4'h3,
       EP_C14ChipNum_SMM =                4'h4,
       EP_C14ChipNum_SMM2 =               4'h5,
       EP_C14ChipNum_SRP =                4'h6,
       EP_C14ChipNum_SPP =                4'h7,
       EP_C14ChipNum_RNP =                4'h8;
  endmodule
// New File: <autoasciienum_param.v>

module sm ();

   localparam STATES = 7;

   localparam /* synopsys enum states */
     IDLE         = 0, // '001
     READ         = 1, // '002
     THINK        = 2, // '004
     SEND         = 3, // '008
     WAIT         = 4, // '040
     GET_ACK      = 5, // '080
     WAIT_REGBUS  = 6; // '100
   
   reg [STATES-1:0] /*synopsys enum states*/
             state_i, state_r; /* synopsys state_vector state_r */
   
   /*AUTOASCIIENUM("state_r","state_onehot,ascii_r","","onehot")*/

   /*AUTOASCIIENUM("state_r","state_notonehot_ascii_r")*/

endmodule
// New File: <autoasciienum_reed.v>
module sm (/*AUTOARG*/
   // Outputs
   state_r,
   // Inputs
   clk, rst_
   );

   //==================== Constant declarations ==============

   parameter [2:0]	// synopsys enum state_info
		IDLE = 		3'b000,
		SEND =		3'b001,
		WAIT1 = 		3'b010,
		UPDATE1 = 	3'b011,
		WAIT2 = 		3'b100;

   parameter [2:0] 	/* synopsys enum state_info */ UPDATE2 = 3'b101;

   parameter [2:0] 	NOT_A_STATE_ELEMENT = 3'b101;

   parameter [2:0] 	/* synopsys enum other */
		A_OTHER_STATE_ELEMENT = 3'b101;

   //==================== Input Signals ======================

   input 		clk;			// System clock signal
   input 		rst_;

   //==================== Output Signals =====================

   // s_ynopsys requires the enum comment between the keyword and the symbol While this seems silly,
   // verilog requires this also to avoid misleading people that also use their tools.

   output [2:0] 	state_r;		// SM State information (to GPF)

   //==================== Intermediate Variables =============

   reg [2:0] 		/* synopsys enum state_info */	state_r;		/* synopsys state_vector state_r */
   reg [2:0] 		/* synopsys enum state_info */	state_e1;		// next state of state-machine

   //==================== Code Begin =========================

   always @(/*AUTOSENSE*/state_r) begin
      case(state_r) // synopsys full_case parallel_case
	IDLE: begin
	   state_e1 = SEND;
	end
	SEND: begin
	   state_e1 = WAIT1;
	end
	WAIT1: begin
	   state_e1 = UPDATE1;
	end
	UPDATE1: begin
	   state_e1 = UPDATE2;
	end
	WAIT2: begin
	   state_e1 = UPDATE2;
	end

	UPDATE2: begin
	   state_e1 = IDLE;
	end
	default:	state_e1 = state_r;
      endcase
   end

   always @(posedge clk or negedge rst_) begin
      if (~rst_) begin
	 state_r <= #0 IDLE;
      end
      else begin
	 state_r <= #0 state_e1;
      end
   end

   //==================== DEBUG ASCII CODE =========================

   /*AUTOASCIIENUM("state_r", "_stateascii_r")*/
   // Beginning of automatic ASCII enum decoding
   reg [55:0]		_stateascii_r;		// Decode of state_r
   always @(state_r) begin
      case ({state_r})
	IDLE:     _stateascii_r = "idle   ";
	SEND:     _stateascii_r = "send   ";
	WAIT1:    _stateascii_r = "wait1  ";
	UPDATE1:  _stateascii_r = "update1";
	WAIT2:    _stateascii_r = "wait2  ";
	UPDATE2:  _stateascii_r = "update2";
	default:  _stateascii_r = "%Error ";
      endcase
   end
   // End of automatics

endmodule
// New File: <autoasciienum_sm.v>
module testit;
   reg clk;
   reg a_i;
   reg b_i;

   reg a_o;
   reg b_o;
   reg rst_n;

   reg [7:0] shreg;


   //== State enumeration
   parameter [2:0] // synopsys enum state_info
     SM_IDLE  = 3'b000,
     SM_SEND  = 3'b001,
     SM_WAIT1 = 3'b010;
   //== State variables
   reg [2:0] /* synopsys enum state_info */
             state_r;                /* synopsys state_vector state_r */
   reg [2:0] /* synopsys enum state_info */
             state_e1;

   //== ASCII state decoding

   /*AUTOASCIIENUM("state_r", "_stateascii_r", "sm_")*/
   // Beginning of automatic ASCII enum decoding
   reg [39:0] _stateascii_r;		// Decode of state_r
   always @(state_r) begin
      case ({state_r})
	SM_IDLE:  _stateascii_r = "idle ";
	SM_SEND:  _stateascii_r = "send ";
	SM_WAIT1: _stateascii_r = "wait1";
	default:  _stateascii_r = "%Erro";
      endcase
   end
   // End of automatics

   initial begin
      clk = 0;
      a_i = 0;
      b_i = 0;
      rst_n = 0;
      #20 rst_n = 1;
   end
   always #5 clk = ~clk;

   always @(posedge clk or rst_n) begin
      if (~rst_n) begin
	 a_o <= 0;
	 b_o <= 0;
	 shreg <= 8'b00110011;
      end
      else begin
	 a_o <= a_i;
	 b_o <= b_i;
	 shreg <= {shreg[6:0], shreg[7]};
      end
   end

   task set_a_i;
      begin
	 a_i = shreg[0];
      end
   endtask // set_a_i

   always @(shreg & a_o) begin
      set_a_i;
   end

   initial begin
      $vcdpluson;
      #500 $finish;
   end

endmodule // testit
// New File: <autoascii_myers.v>
localparam [3:0] /* synopsys enum xstate */
  state0 = 4'h0,
  state1 = 4'h1;
// New File: <autoascii_peltan_inc.v>
module autoascii_peltan
  (
   input        test,
   output [1:0] test_out
   );

`include "autoascii_peltan_inc.v"

   // Duplicate of what's in _inc
   localparam [3:0] /* synopsys enum xstate */
	    state0 = 4'h0;

   wire [3:0] 	/* synopsys enum xstate */
            xstate;

   /* synopsys translate off */

   /*AUTOASCIIENUM("xstate", "v_xstate")*/
   // Beginning of automatic ASCII enum decoding
   reg [47:0]		v_xstate;		// Decode of xstate
   always @(xstate) begin
      case ({xstate})
	state0:   v_xstate = "state0";
	state1:   v_xstate = "state1";
	default:  v_xstate = "%Error";
      endcase
   end
   // End of automatics

   /* synopsys translate on */

endmodule // sample

// Local Variables:
// verilog-library-directories:(".")
// verilog-auto-read-includes:t
// End:
// New File: <autoascii_peltan.v>
module autoconstant_gooch
  (/*AUTOARG*/
   // Outputs
   out1, out2, out3,
   // Inputs
   in1, in2, in3
   );

   input	[3:0]	in1;
   input [3:0] 		in2;
   input [3:0] 		in3;
   output [3:0] 	out1;
   reg [3:0] 		out1;
   output [3:0] 	out2;
   reg [3:0] 		out2;
   output [3:0] 	out3;
   reg [3:0] 		out3;



   always @(/*AUTOSENSE*/in1 or in2 or in3)
     begin
	case (in1)
	  4'b0001 :	begin
	     out1 = in2;
	  end
	  4'b0010 :	begin
	     out1 = in2 + in3;
	  end
	  4'b0100 :	begin
	     out1 = in2 - in3;
	  end
	  4'b1000 :	begin
	     out1 = in2;
	  end
	  default	:	begin
	     out1 = {4{1'b0}};
	  end
	endcase
     end


   always @(/*AUTOSENSE*/in1 or in2 or in3)
     begin
	case (in1)
	  4'b0001 :	begin
	     out2 = in2;
	  end
	  4'b0010 :	begin
	     out2 = in2 + in3;
	  end
	  4'b0100 :	begin
	     out2 = in2 - in3;
	  end
	  4'b1000 :	begin
	     out2 = in2;
	  end
	  default	:	begin
	     out2 = {4{1'b0}};
	  end
	endcase
     end


   always @(/*AUTOSENSE*/in1 or in2 or in3)
     begin
	/* AUTO_CONSTANT( temp )*/
	/* AxxxUTO_CONSTANT temp */
	out3 = in1 + in2;
	temp2 = temp;

	// ERROR here - above constant definition is not
	// correct - no braces - and so parser keeps looking
	// for the first variable it finds between a pair of
	// braces - in this case, in2. This in2 is now a
	// "constant" and is removed from all sensitivity lists.
	// ( in2 )

	case (in1)
	  4'b0001 :	begin
	     out3 = in2;
	  end
	  4'b0010 :	begin
	     out3 = in2 + in3;
	  end
	  4'b0100 :	begin
	     out3 = in2 - in3;
	  end
	  4'b1000 :	begin
	     out3 = in2;
	  end
	  default	:	begin
	     out3 = {4{1'b0}};
	  end
	endcase
     end



endmodule


// New File: <autoconstant_gooch.v>
module(/*AUTOARG*/)


  always @(`ot.BOZ or
	   /*AUTOSENSE*/b)
    begin
       /*AUTO_CONSTANT(`ot.BOC) */
       i = b;
       c = `ot.BOC;
       d = `ot.BOZ;
    end

   always @(/*AUTOSENSE*/b)
     begin
	/*AUTO_CONSTANT(ot.BOB) */
	i = b;
	c = ot.BOB;
     end

endmodule
// New File: <autoconst_gesmith.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2011 by Wilson Snyder.

// Note intentional whitespace on following line
//       SPACES
module x;      
   //       TAB
endmodule	

// Local Variables:
// verilog-auto-delete-trailing-whitespace: t
// End:
// New File: <auto_delete_whitespace.v>
module autoinoutmodule (/*AUTOARG*/);

   output lower_inb;

   /*AUTOINOUTCOMP("inst")*/
   // Beginning of automatic in/out/inouts (from specific module)

   wire   lower_out = lower_ina | lower_inb;

endmodule

// New File: <autoinoutcomp.v>
module autoinoutin (/*AUTOARG*/);

   /*AUTOINOUTIN("inst")*/

endmodule

// New File: <autoinoutin.v>
// Matthew Lovell <lovell@hp.com>

module top_test(/*AUTOARG*/);

   /*AUTOOUTPUT*/

   /*AUTOINPUT*/

   /* leaf AUTO_TEMPLATE (
    // Original reported bug
    .a  ({aa, test2.test3.no, test3.test4.no2}),
    // Others
    .b  ( ~ ba),
    .c  ({\c-escaped-nvec , \c-escaped-vec [2:0] }),
    .d  ({da,~ db [1:0] , dc [2:0]}),
    // Msg246
    .e ({{4*1{1'b0}},Z_int[15:0],{1'b4{1'b0}},Z0_int[7:0]}),
    .f (hi.ear.ial),
    );
    */
   leaf l1 (/*AUTOINST*/);
endmodule // top_test

module leaf(/*AUTOARG*/);
   output a;
   output  b;
   input c;
   input d;
   input e;
   input f;
   input z;
endmodule
// New File: <autoinout_lovell.v>
module foo
  (
   input  soutb,
   output sina,

   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
   ) ;
  bar i_bar(/*AUTOINST*/);

endmodule // foo

module bar (/*AUTOARG*/) ;
   input sina, sinb;
   output souta, soutb;
endmodule // bar
// New File: <autoinout_ma.v>
//-*- mode: Verilog; verilog-indent-level: 3; indent-tabs-mode: nil; tab-width: 1 -*-

module apl2c_connect(autoinoutmodport_type_intf ctl_i,
   /*AUTOINOUTMODPORT("autoinoutmodport_type_intf",  "ctl_cb")*/
);

   /*AUTOASSIGNMODPORT("autoinoutmodport_type_intf", "ctl_cb", "ctl_i")*/

endmodule

interface autoinoutmodport_type_intf(input logic clk, input logic rst_n);
   import uvm_pkg::*;
   import ap_defines::*;

   logic [4:0]  inst;
   isel_t       isel;
   logic        replay;

   clocking ctl_cb @(posedge clk);
      input inst;
      input isel;
      input replay;
   endclocking: ctl_cb

   modport ctl_mp(clocking ctl_cb);

endinterface

// Local Variables:
// verilog-typedef-regexp:"_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <autoinoutmodport_bourduas_type.v>
interface ExampIf
  ( input logic clk );
   logic        req_val;
   logic [7:0]  req_dat;
   logic        out;
   clocking mon_clkblk @(posedge clk);
      input     req_val;
      input     req_dat;
      output	out;
   endclocking
   modport mp(clocking mon_clkblk);
endinterface


module ExampMain
( input clk,

  // Manually declared, so make sure not redeclared
  // Note this is AFTER addition of the prefix
  input a_req_dat,

  /*AUTOINOUTMODPORT("ExampIf", "mp", "", "a_")*/
  /*AUTOINOUTMODPORT("ExampIf", "mp", "", "b_")*/
);

   ExampleIf ia;
   ExampleIf ib;

   // Manually declared (duplications are NOT detected yet)
   assign a_out = 1;

   /*AUTOASSIGNMODPORT("ExampIf", "mp", "ia", "", "a_")*/
   /*AUTOASSIGNMODPORT("ExampIf", "mp", "ib", "", "b_")*/

endmodule
// New File: <autoinoutmodport_prefix.v>
// See also autoinst_defs

`define foo 6
`define bar 0

module autoinoutmodule_dedefine(
   /*AUTOINOUTMODULE("bar")*/
   );
endmodule

module bar;
   output onewide;
   output [3:1] fourwide;
   output [`foo-1:`bar] varwide;
endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:

// New File: <autoinoutmodule_dedefine.v>
module autoinoutmodule_iface_sub
  (my_svi.master my_svi_port,
   );
endmodule
// New File: <autoinoutmodule_iface_sub.v>
// bug721
module my_core
  (
   /*AUTOINOUTMODULE("autoinoutmodule_iface_sub")*/
   /*AUTOINOUT*/
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
);
/*AUTOWIRE*/

endmodule
// New File: <autoinoutmodule_iface.v>
module autoinoutmodule (/*AUTOARG*/);

   /*AUTOINOUTMODULE("inst","\(ina\|out\)","","ina")*/

   wire   lower_out = lower_ina;

endmodule

// New File: <autoinoutmodule_ign.v>
module my_dut(
   input clk,
   input [3:0][7:0] data_in,
   output [3:0][7:0] data_out);
endmodule
 
module shell(
   /*AUTOINOUTMODULE("my_dut","","^input")*/
   );
endmodule
// New File: <autoinoutmodule_multidim.v>
module autoinoutmodule_re2 (/*AUTOARG*/);

   /*AUTOINOUTMODULE("inst","","input.*")*/

   /*AUTOINOUTMODULE("inst","","output.*")*/

   wire   lower_out  = lower_ina;

endmodule

// New File: <autoinoutmodule_re2.v>
module autoinoutmodule (/*AUTOARG*/);

   /*AUTOINOUTMODULE("inst","\(ina\|out\)")*/

   wire   lower_out = lower_ina;

endmodule

// New File: <autoinoutmodule_regexp.v>
module autoinoutmodule (/*AUTOARG*/
   // Outputs
   lower_out,
   // Inputs
   lower_inb, lower_ina
   );

   /*AUTOINOUTMODULE("inst")*/
   // Beginning of automatic in/out/inouts (from specific module)
   output		lower_out;
   input		lower_inb;
   input		lower_ina;
   // End of automatics
   // Beginning of automatic in/out/inouts (from specific module)

   wire   lower_out = lower_ina | lower_inb;

endmodule

// New File: <autoinoutmodule.v>
module autoinoutmodule (
   /*AUTOINOUTMODULE("inst")*/
   );

   wire   lower_out = lower_ina | lower_inb;

endmodule

// New File: <autoinoutmodule_v2k.v>
module a ( /*AUTOARG*/
   // Outputs
   o1,
   // Inouts
   io1,
   // Inputs
   i1
   ) ;

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		i1;			// To b of b.v
   // End of automatics

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		o1;			// From b of b.v
   // End of automatics

   /*AUTOINOUT*/
   // Beginning of automatic inouts (from unused autoinst inouts)
   inout		io1;			// To/From b of b.v
   // End of automatics

   b b
     (/*AUTOINST*/
      // Outputs
      .o1				(o1),
      // Inouts
      .io1				(io1),
      // Inputs
      .i1				(i1));

endmodule // a

module b (/*AUTOARG*/
   // Outputs
   o1,
   // Inouts
   io1,
   // Inputs
   i1
   ) ;

   input i1 ;
   output o1 ;
   inout  io1 ;

endmodule // b
// New File: <autoinout_moller.v>
module autoinoutparam (/*AUTOARG*/);

   /*AUTOINOUTPARAM("inst","param1")*/

   /*AUTOINOUTPARAM("inst")*/

endmodule

// New File: <autoinoutparam.v>
module io1_sub(
	       /*AUTOARG*/);

   /*AUTOINPUT("^s")*/

   /*AUTOINOUT("^s")*/

   /*AUTOOUTPUT("^s")*/

   /* inst AUTO_TEMPLATE (
    .lower_inb		(1'b1),
    )*/


   instio instio (/*AUTOINST*/);

endmodule

module instio (/*AUTOARG*/);

   input lower_ina;
   inout lower_io;
   output lower_out;
   input sec_ina;
   inout sec_io;
   output sec_out;

   wire   lower_out = lower_ina | lower_io;
   wire   sec_out = sec_ina | sec_io;

endmodule

// New File: <autoinout_regexp.v>
module io1_sub(
	       /*AUTOARG*/);

   /*AUTOINPUT*/

   /*AUTOINOUT*/

   /*AUTOOUTPUT*/

   /* inst AUTO_TEMPLATE (
    .lower_inb		(1'b1),
    )*/


   instio instio (/*AUTOINST*/);

endmodule

module instio (/*AUTOARG*/);

   input lower_ina;
   inout lower_io;
   output lower_out;
   input sec_ina;
   inout sec_io;
   output sec_out;

   wire   lower_out = lower_ina | lower_io;
   wire   sec_out = sec_ina | sec_io;

endmodule

// New File: <autoinout.v>
module parent
  (
  
   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOINOUT*/
   );

  
  /*AUTOWIRE*/

  
  child_a sub_module
    (/*AUTOINST*/);

  child_b sub_module
    (/*AUTOINST*/);  
  
endmodule // parent

module child_a
  (
   input [3:0]   a,
   input [3:0]   a2,
   input [3:0]   a3,
   input [3:0]   a4,   
   inout [5:0]   c,
   inout [5:0]   c2,
   inout         c3,
   output [2:0]  b,
   output [2:0]  b2,
   output [2:0]  b3,
   output [2:0]  b4,
   input         clk,
   output        d
   );
endmodule

  
module child_b
  (
   output [3:0]   a,
   output [3:0]   a4,   
   inout [5:0]    c,
   inout [5:0]    c2,
   inout          c3,
   input [2:0]    b,
   input [2:0]    b2,
   input [2:0]    b3,
   input [2:0]    b4,
   input          clk
   );  
endmodule
// New File: <autoinout_v2k.v>
module try_top(/*autoarg*/
   // Outputs
   out_momo,
   // Inputs
   in_momo
   );

   //bug1105 - should be [1:0]

   /*autoinput*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input [0] [127:0]	in_momo;		// To try0 of try1.v, ...
   // End of automatics
   /*autooutput*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output [0] [127:0]	out_momo;		// From try0 of try1.v, ...
   // End of automatics


   /*  try1 auto_template "try\(.*\)" 
    (
    .out_momo                      (out_momo[@][]),
    .in_momo                       (in_momo[@][]),
    );*/

   
   try1 try0 (/*autoinst*/
	      // Outputs
	      .out_momo			(out_momo[0][127:0]),	 // Templated
	      // Inputs
	      .in_momo			(in_momo[0][127:0]));	 // Templated
   try1 try1 (/*autoinst*/
	      // Outputs
	      .out_momo			(out_momo[1][127:0]),	 // Templated
	      // Inputs
	      .in_momo			(in_momo[1][127:0]));	 // Templated


endmodule // try_top

module try1(/*autoarg*/
   // Outputs
   out_momo,
   // Inputs
   in_momo
   );

   input [127:0] in_momo;
   output [127:0] out_momo;
endmodule // try1
// New File: <autoinput_2d_gaspar.v>
module mod1(input logic [1:0] reg1[4],
            input logic       reg2[5][6],
            input logic [1:0] [3:0] [2:0] reg4);
endmodule

module mod2(output logic [1:0] reg1[4],
	    output logic [1:0] [3:0] [2:0] reg4);
endmodule

module dut (
	    /*AUTOINPUT*/
            /*AUTOOUTPUT*/
            );

   /*AUTOWIRE*/

   mod1 foo_i(/*AUTOINST*/);

   /* drv_i AUTO_TEMPLATE (.reg1(reg1[]), );*/
   mod2 drv_i(/*AUTOINST*/);
endmodule
// New File: <autoinput_array_bug294.v>
module io1_sub(
	       /*AUTOARG*/
   // Outputs
   lower_out,
   // Inputs
   lower_ina
   );

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		lower_ina;		// To inst of inst.v
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		lower_out;		// From inst of inst.v
   // End of automatics

   /* inst AUTO_TEMPLATE (
    .lower_inb		(1'b1),
    )*/


   inst inst (/*AUTOINST*/
	      // Outputs
	      .lower_out		(lower_out),
	      // Inputs
	      .lower_inb		(1'b1),			 // Templated
	      .lower_ina		(lower_ina));

endmodule
// New File: <autoinput_asharma.v>
module io1_sub(
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
   );

   /* inst AUTO_TEMPLATE (
    .lower_inb		(1'b1),
    )*/


   inst inst (/*AUTOINST*/);

endmodule
// New File: <autoinput_asharma_v2k.v>
module xyz (/*AUTOARG*/);

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   /* abc AUTO_TEMPLATE
     (
      // Outputs
      .signal_c				(signal_c),
      // Inputs
      .signal_a				({1'b0, signal_f}),
      .signal_b				(signal_b[2:0]));
    */
   
   abc u_abc
     (/*AUTOINST*/);

   /* def AUTO_TEMPLATE
     (// Outputs
      .signal_f				(signal_f),
      // Inputs
      .signal_d				({1'b1, signal_c}),
      .signal_e				({2'b11, signal_e}),
      .signal_e2			(({2'b11, signal_e2})),
      .signal_e3			((signal_e3)) );
    */
    
   def u_def
     (/*AUTOINST*/);
   
endmodule // xyz

module abc (/*AUTOARG*/);

   input [1:0] signal_a;
   input [2:0] signal_b;
   output signal_c;

endmodule // abc

module def (/*AUTOARG*/);

   input [1:0] signal_d;
   input [2:0] signal_e;
   input [3:0] signal_e2;
   input [3:0] signal_e3;
   output signal_f;

endmodule // def

// Local Variables:
// verilog-auto-ignore-concat: t
// End:
// New File: <autoinput_concat_ignore.v>
module xyz (/*AUTOARG*/);

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   /* abc AUTO_TEMPLATE
     (
      // Outputs
      .signal_c				(signal_c),
      // Inputs
      .signal_a				({1'b0, signal_f}),
      .signal_b				(signal_b[2:0]));
    */
   
   abc u_abc
     (/*AUTOINST*/);

   /* def AUTO_TEMPLATE
     (// Outputs
      .signal_f				(signal_f),
      // Inputs
      .signal_d				({1'b1, signal_c}),
      .signal_e				({2'b11, signal_e}));
    */
    
   def u_def
     (/*AUTOINST*/);
   
endmodule // xyz

module abc (/*AUTOARG*/);

   input [1:0] signal_a;
   input [2:0] signal_b;
   output signal_c;

endmodule // abc

module def (/*AUTOARG*/);

   input [1:0] signal_d;
   input [2:0] signal_e;
   output signal_f;

endmodule // def
// New File: <autoinput_concat_lau2.v>
module sub1 (/*AUTOARG*/);

   input [3:0] bus1;
   inout [3:0] busout;

   wire 	busout = bus1;

endmodule


module autoinput_concat_lau
  (
   /*AUTOINPUT*/
   /*AUTOINOUT*/
   );

   /* sub1 AUTO_TEMPLATE (
	      .busout	({2'b0,fooout[1:0]}),
	      .bus1	({2'b0,~foo[1:0]}),
    );
    */

   sub1 sub1 (/*AUTOINST*/);

endmodule

// New File: <autoinput_concat_lau.v>
module xyz (/*AUTOARG*/
   // Outputs
   signal_f, signal_c,
   // Inputs
   signal_e3, signal_b
   );

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input [2:0]		signal_b;		// To u_abc of abc.v
   input		signal_e3;		// To u_def of def.v
   // End of automatics

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		signal_c;		// From u_abc of abc.v
   output		signal_f;		// From u_def of def.v
   // End of automatics

   /*AUTOWIRE*/

   /* abc AUTO_TEMPLATE
     (
      // Outputs
      .signal_c				(signal_c),
      // Inputs
      .signal_a				(signal_f),   // AUTONOHOOKUP
      .signal_b				(signal_b[2:0]));
    */
   
   abc u_abc
     (/*AUTOINST*/
      // Outputs
      .signal_c				(signal_c),		 // Templated
      // Inputs
      .signal_a				(signal_f),		 // Templated AUTONOHOOKUP
      .signal_b				(signal_b[2:0]));	 // Templated

   /* def AUTO_TEMPLATE
     (// Outputs
      .signal_f				(signal_f),
      // Inputs
      .signal_d				(signal_c),   // AUTONOHOOKUP
      .signal_e				(signal_e),   // AUTONOHOOKUP
      .signal_e2			(signal_e2),   // AUTONOHOOKUP
      .signal_e3			((signal_e3)),
      );
    */
    
   def u_def
     (/*AUTOINST*/
      // Outputs
      .signal_f				(signal_f),		 // Templated
      // Inputs
      .signal_d				(signal_c),		 // Templated AUTONOHOOKUP
      .signal_e				(signal_e),		 // Templated AUTONOHOOKUP
      .signal_e2			(signal_e2),		 // Templated AUTONOHOOKUP
      .signal_e3			((signal_e3)));		 // Templated
   
endmodule // xyz

module abc (/*AUTOARG*/
   // Outputs
   signal_c,
   // Inputs
   signal_a, signal_b
   );

   input [1:0] signal_a;
   input [2:0] signal_b;
   output signal_c;

endmodule // abc

module def (/*AUTOARG*/
   // Outputs
   signal_f,
   // Inputs
   signal_d, signal_e, signal_e2, signal_e3
   );

   input [1:0] signal_d;
   input [2:0] signal_e;
   input [3:0] signal_e2;
   input [3:0] signal_e3;
   output signal_f;

endmodule // def
// New File: <autoinput_nohookup.v>
// Julian Gorfajn

`default_nettype none

module top
  (
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output wire		o,			// From sub of Sub.v
   // End of automatics
   // Beginning of automatic outputs (from unused autoinst outputs)
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input wire		i			// To sub of Sub.v
   // End of automatics
   );
   
   Sub sub (/*AUTOINST*/
	    // Outputs
	    .o				(o),
	    // Inputs
	    .i				(i));
endmodule

module Sub (input i, output o);
endmodule

// Local Variables:
// verilog-auto-declare-nettype: "wire"
// End:
// New File: <autoinput_none.v>
module foo (/*AUTOARG*/) ;
  /*AUTOINPUT("ab\(c\|d\)")*/
  bar i_bar(/*AUTOINST*/);

endmodule // foo

module bar (/*AUTOARG*/) ;
  input abc,abd,bca;
  output aaa;
endmodule // bar
// New File: <autoinput_paren.v>
module ExampInsertLisp;

   /*AUTOINSERTLAST(my-verilog-insert-hello "world")*/

endmodule
/*
 Local Variables:
 eval:
   (defun my-verilog-insert-hello (who)
     (insert (concat "initial $write(\"hello " who "\");\n")))
 End:
*/
// New File: <autoinsertlast_1.v>
module top ();


   sub sub(/*AUTOINST*/
	   // Outputs
	   .b				(b[PARAM2:0]),
	   // Inputs
	   .a				(a[PARAM1:0]));

endmodule // top

module sub
  #(parameter PARAM1=2,
    PARAM2=3,
    PARAM3=6)
     ( input wire [PARAM1:0] a,
       output reg [PARAM2:0]   b
      );


endmodule
// New File: <autoinst_2k_fredriksen.v>
module autoinst_ams_vorwerk;

  latch latch (/*AUTOINST*/);

endmodule

module latch (/*AUTOARG*/);

`ifdef __VAMS_ENABLE__
    output (* integer groundSensitivity="gnd "; integer supplySensitivity="vdd "; *) q;
 `else
    output q;
`endif

`ifdef __VAMS_ENABLE__
    input (* integer groundSensitivity="gnd "; integer supplySensitivity="vdd "; *) en;
 `else
    input en;
`endif

`ifdef __VAMS_ENABLE__
    input (* integer groundSensitivity="gnd "; integer supplySensitivity="vdd "; *) d;
 `else
    input d;
`endif

endmodule
// New File: <autoinst_ams_vorwerk.v>
module sub(output logic [1:-1] oned,
           output logic [1:-1] [2:-1] 	     twod,
           output logic [1:-1] [2:-1] [3:-3] threed);
endmodule

module dut (
            );

   /*AUTOWIRE*/

   /* sub AUTO_TEMPLATE ();
   */

   sub sub1 (/*AUTOINST*/);

   /* sub AUTO_TEMPLATE (
	     .oned			(b_oned[]),
	     .twod			(b_twod[]),
	     .threed			(b_threed[]));
   */

   // NOTE this results in the wrong declaration for b_twod/b_threed
   sub subb (/*AUTOINST*/);

   /* sub AUTO_TEMPLATE (
	     .oned			(c_oned[]),
	     .twod			(c_twod[x][]),
	     .threed			(c_threed[x][y][]));
   */

   sub subc (/*AUTOINST*/);

   /* sub AUTO_TEMPLATE (
	     .oned			(d_oned[][]),
	     .twod			(d_twod[][]),
	     .threed			(d_threed[][]));
   */

   sub subd (/*AUTOINST*/);

endmodule
// New File: <autoinst_array_braket.v>
// bug637

module submod_a
  (
   //Inputs
   input wire signed [15:0]  serial_in,
   //Outputs
   output wire signed [15:0] parallel_out [0:7]
   );
endmodule

module submod_b
  (
   //Inputs
   input wire signed [15:0]  parallel_out [0:7],
   //Outputs
   output wire signed [15:0] final_out [0:7]
   );
endmodule

module top
  (
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
   );

   /*AUTOLOGIC*/

   submod_a a_inst
     (/*AUTOINST*/);

    submod_b b_inst
      (/*AUTOINST*/);

    
endmodule
// New File: <autoinst_array.v>
module ex;

   /* autoinst_paramover_sub AUTO_TEMPLATE "u_\(.*\)" (
    .a(inA_@[]),
    .b(outA_@[]),
    );*/

   autoinst_paramover_sub u_foo(/*AUTOINST*/);
   autoinst_paramover_sub u_bar(/*AUTOINST*/);
   autoinst_paramover_sub u_baz(/*AUTOINST*/);

   /* autoinst_paramover_sub AUTO_TEMPLATE (
    .a(inN_@[]),
    .b(outN_@[]),
    );*/

   autoinst_paramover_sub u_0_2(/*AUTOINST*/);
   autoinst_paramover_sub u_1_3(/*AUTOINST*/);

endmodule
// New File: <autoinst_atregexp.v>
module autoinst_autonohookup (
                              /*AUTOINPUT*/
                              /*AUTOOUTPUT*/
                              // Beginning of automatic outputs (from unused autoinst outputs)
                              output o1                 // From a2 of a2.v
                              // End of automatics
                              );
   /* a2 AUTO_TEMPLATE (
    .i1(i1), // AUTONOHOOKUP
    .o1(o1),
    ) */
   a2 a2( /*AUTOINST*/
          // Outputs
          .o1                           (o1),                    // Templated
          // Inputs
          .i1                           (i1));                   // Templated AUTONOHOOKUP
endmodule

module a2 (
           input  i1,
           output o1
           );
endmodule

// New File: <autoinst_autonohookup.v>
module aaa (/*AUTOARG*/);

   /*AUTOOUTPUT*/
   /*AUTOINPUT*/
   /*AUTOWIRE*/

   wire u0, u1, z0, z1;
   /*
    bbb AUTO_TEMPLATE (
            .xo0 ({(u0), y0}),
            .xo1 ({y1, (u1)}),
            .xi0 ({(z0), y2}),
            .xi1 ({y3, (z1)}),
    );
    */
   bbb bbb (/*AUTOINST*/);

endmodule // aaa

module bbb (/*AUTOARG*/);
   output [1:0] xo0, xo1;
   input [1:0]  xi0, xi1;
   /*AUTOTIEOFF*/
endmodule // bbb
// New File: <autoinst_belkind_concat.v>
module autoinst_bits_lba_gi

  // ==xxxxxxx==

  // xxxxxxxxx 1997-1998, xxxxx xxx.
  // xxx xxxxxx xxxxxxxx

  // ****************************************************************** /
  // ****************************************************************** /
  // xxxxx, xxxxxxxxxxxx
  // xxxxxxx:		xxx-4080
  // xxxxxx:  		xxx xxxxxxx
  // xxxxx:		xxxxxxx 16, 1998
  // ******************************************************************* /
  // ******************************************************************* /
  // xxxx xxxx:		xxx_xx.x
  // xxxxxxxx xxxxxxx:
  // $xxx: xxx_xx.x,x $
  // xxxxxxxx 1.3  1998/03/06 00:27:00  xxx
  // -- xxxxx xxxxxxx xx xx.xxxx xxx xxxxxxxx xxxxxxx.
  // -- xxxx xxxxxxxxxxxxx xx xx xxxxxxxxxxx xx xx.x xxx xx.x xx xxxxxxx xxx xxxxxxxxxxx.
  // -- xxxx xxxxx xxx xxxxxxxxxxxxx xxx1'x (xxxxxxx xx xxxxx) xx xx.x xxx xxxx xxxxxx
  //    xxxxxxxxxx.
  // -- xxxxx xxxxxxxxxx xxxxxxxx xx xxx xx xxxxxxxxxxx xxx xxxx xxxxxx xxxxxxxxxxxx xxxx.
  // -- xx xxx xxxxxxxxx xxx xxxxxx xx xx xx xxx xxxxx xxxxx xx xxxx xxxxxxxx xx xxxx.
  // -- xx xx xxxx xxxxxxx xxx xxx xxxx xxxxxx xxxxxx (xxx xxx xxxx) xxx xxx xxxx/xxxx
  //    xxxxxxxxxx xxxx xxxxxxx.
  //
  // xxxxxxxx 1.2  1998/03/04 18:58:55  xxx
  // xxxxx xxxxxxxxx xxx-xx xxxxxxx xxxxxxx xx xxxx xxxxx.
  //
  // xxxxxxxx 1.1  1998/02/23 19:31:52  xxx
  // xxxxx xxxxx xxxxxx xxx xxxx xxxxx xxxxx xxxxxxxxxx.
  //
  // ---------------------------------------------------------------
  //
  // xxxxxxx xxx xxxxxxxxx xxx xxxxx xxx xxxxxxx xxx
  //
  // xxxx xxxxxx xxxxx xxx xxxxx xxxxxx xxxxxxx/xxxx xxx xxxx
  //      xx.x xxx xx xx xxxxxxxxx xxxx xxxx.x xxxxxx xx
  //      xxxx xxx xxxxxxx xx xxx. xx xxxx xxxxates the
  //      bidir Foo Bus into a chip input for use by li.v
  //
  // This module also isolates for input to lbsm.v, and drives
  //       (if so indicated by lbsm.v) the bidir Fooileo Cmd bus.
  //
  //

  (
   CLK,
   WWADoe,
   WWCoe,
   WWCmdIfOE,
   WWADHold,
   iWWADO,
   WWCmdI,
   WWADI,
   WWADB,
   WWCmdB
   );

   /////////////////////////////////////////////////////////////////////
	 // inputs

   input		CLK;		// LBA clk

   // inputs from lbsm.v
   input		WWADoe;		// FooBus Addr/Data OE
   input		WWCoe;		// FooBus Cmd OE
   input [8:0] 		WWCmdIfOE;	// FooBus Cmd if enabled
   input		WWADHold;	// FooBus Addr hold

   // inputs from li.v
   input [31:0] 	iWWADO;		// FooBus Address/Data out next cycle


   /////////////////////////////////////////////////////////////////////
   // outputs

   // outputs to lbsm.v
   output [8:0] 	WWCmdI;		// FooBus Command in

   // outputs to li.v
   output [31:0] 	WWADI;		// FooBus Address/Data in


   /////////////////////////////////////////////////////////////////////
   // bidirs

   // bidirs to/from off-chip
   inout [31:0] 	WWADB;		// bidir FooBus addr/data
   inout [8:0] 		WWCmdB;		// bidir FooBus command

   /////////////////////////////////////////////////////////////////////
   // reg's for outputs (some flops, some not)

   /////////////////////////////////////////////////////////////////////
   // other flops

   reg [31:0] 		WWADIfOE;	// FooBus Addr/Data Out if enabled



endmodule


// Local Variables:
// eval:(if (fboundp `verilog-enable-indentation) (verilog-enable-indentation))
// End:
// New File: <autoinst_bits_lba_gi.v>
module lba

  (/*AUTOnotARG*/
   // Outputs
   );


   /* autoinst_bits_lba_gi	AUTO_TEMPLATE	(
    .WWCmdI	(WWCmdI[]));
    */

   autoinst_bits_lba_gi	gi	(/*AUTOINST*/
				 // Outputs
				 .WWCmdI		(WWCmdI[8:0]),	 // Templated
				 .WWADI			(WWADI[31:0]),
				 // Inouts
				 .WWADB			(WWADB[31:0]),
				 .WWCmdB		(WWCmdB[8:0]),
				 // Inputs
				 .CLK			(CLK),
				 .WWADoe		(WWADoe),
				 .WWCoe			(WWCoe),
				 .WWCmdIfOE		(WWCmdIfOE[8:0]),
				 .WWADHold		(WWADHold),
				 .iWWADO		(iWWADO[31:0]));

endmodule
// New File: <autoinst_bits_lba.v>
module andcell (/*AUTOARG*/
   // Outputs
   c,
   // Inputs
   a, b
   );

   input a;
   input b;
   output c;
   wire   c = a&b;
endmodule

module nandcell (/*AUTOARG*/
   // Outputs
   c,
   // Inputs
   a, b
   );

   input a;
   input b;
   output c;
   wire   c = !(a&b);
endmodule

module orcell (/*AUTOARG*/
   // Outputs
   c,
   // Inputs
   a, b
   );

   input a;
   input b;
   output c;
   wire   c = a|b;
endmodule
// New File: <autoinst_brucet_library.v>
module io1_sub(/*AUTOARG*/
   // Outputs
   x, y, z,
   // Inputs
   a, b
   );

   input a;
   input b;
   output x;
   output y;
   output z;

   andcell c0 (
	       .c			(x),
	       /*AUTOINST*/
	       // Inputs
	       .a			(a),
	       .b			(b));

   orcell c0 (
	      .c			(y),
	      /*AUTOINST*/
	      // Inputs
	      .a			(a),
	      .b			(b));

   nandcell c0 (
		.c			(z),
		/*AUTOINST*/
		// Inputs
		.a			(a),
		.b			(b));

endmodule

// Local Variables:
// verilog-library-files:("autoinst_brucet_library.v")
// End:
// New File: <autoinst_brucet.v>
typedef struct packed {
   logic [7:0] data;
   logic       wr_ena;
} mystruct_s;

module submod
    (input logic a_port,
     input logic [4:0] b_bus,
     input             mystruct_s single_struct_is_fine,
     input             mystruct_s [2:0] array_of_struct_is_not,
     output logic      status);

   /*AUTOTIEOFF*/
   // Beginning of automatic tieoffs (for this module's unterminated outputs)
   wire			status			= 1'h0;
   // End of automatics

endmodule // submod

module top;
   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic		status;			// From submod0 of submod.v
   // End of automatics

   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   logic		a_port;			// To submod0 of submod.v
   mystruct_s [2:0]	array_of_struct_is_not;	// To submod0 of submod.v
   logic [4:0]		b_bus;			// To submod0 of submod.v
   mystruct_s		single_struct_is_fine;	// To submod0 of submod.v
   // End of automatics

   submod submod0
       (/*AUTOINST*/
	// Outputs
	.status				(status),
	// Inputs
	.a_port				(a_port),
	.b_bus				(b_bus[4:0]),
	.single_struct_is_fine		(single_struct_is_fine),
	.array_of_struct_is_not		(array_of_struct_is_not[2:0]));
endmodule // top

// Local Variables:
// verilog-typedef-regexp: "_s$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_s\\>"
// End:
// New File: <autoinst_bug373.v>
module foo();
   /*bar AUTO_TEMPLATE (
    .pme\(.*\)_o             (pme\1[]),
    .PME\(.*\)_o             (pMe\1[]),
    .pme\(.*\)_o             (pme\1[]),
    );
    */
   bar bar
     (/*AUTOINST*/);

endmodule

module bar
  (/*AUTOARG*/);

   input PME_o;
   input pme_o;
   input pmE_o;

endmodule
// New File: <autoinst_case_chakradhara.v>
typedef struct packed {
  logic			a,b,c;
} tTest;

module test
  (
   input clk,rst
   );

   wire [7:0] data_tm;

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   tTest		q;			// From foo of foo.v
   // End of automatics

   /* foo AUTO_TEMPLATE (
    .tm			(data_tm),
    );
    */

   foo foo (/*AUTOINST*/
	    // Outputs
	    .q				(q),
	    // Inputs
	    .clk			(clk),
	    .rst			(rst),
	    .tm				(data_tm));		 // Templated
   /*AUTO_LISP(setq verilog-typedef-regexp "^t[A-Z]")*/
endmodule

module foo
  (
   input       clk,
   input       rst,
   input [7:0] tm,
   output      tTest q
   );
endmodule

// Local Variables:
// verilog-case-fold:nil
// verilog-library-directories:(".")
// verilog-typedef-regexp:"^t[A-Z]"
// verilog-align-typedef-words: ("tTest")
// End:
// New File: <autoinst_casefold_hou.v>

module fifo(/*AUTOARG*/);

input     clk;                          
input     rst_n;                        
output    fifo_full_w;                  

input     enqueue_w;                    
input [(DATA_WIDTH-1):0] data_in_w;     

output                   data_valid_r;  
input                    dequeue_w;     
input [1:0]              full_threshold; 

output [(DATA_WIDTH-1):0] rdata_r;      


endmodule

module req (p_clk, carb_rst_rnp, req_rp, len_rxp, deq_req, deq_len, deq_val);

   input p_clk; 
   input carb_rst_rnp; 
   input [4:0] len_rxp; 
   input       req_rp; 
   input       deq_req; 
   output [4:0] deq_len; 
   output 	deq_val; 
   reg [5:0] 	fifo_entry1_rp;
   reg [5:0] 	fifo_entry2_rp;
   reg [4:0] 	deq_len; 
   reg 		deq_val;

endmodule

module pull( /*AUTOARG*/);

   input clk; 
   input rst_rnpha; 
   input [4:0] lenar_rxp; 
   input       rem_rpd; 
   input       d_rews; 
   output [4:0] d_len; 
   output 	d_val; 


/*   req AUTO_TEMPLATE "\(g[a-z0-9]+\|g.*[0-9]\)" (
                             .p_clk (my_clk_@),
                             .len_rxp (carb_rst_rnp_@),
                             .carb_rst_rnp (pull_req1));
 
*/

  req test432_gbe5(/*AUTOINST*/);

   req gbe9_vreos(/*AUTOINST*/);


/*  fifo AUTO_TEMPLATE "gbe[0-9]+_\([^\_]+\)" (
                             .clk (@_clk),
                             .\(.*data.*\) (@_\1),
                             .\(.*\)\(full\)\(.*\) (\1@\3),
                             .\(en\|de\)\(.\).+ (@_\1\2));
*/



   fifo #(5)  gbe2_pull_req (/*AUTOINST*/);


  fifo #(5)  
     gbe1_pull_req_fifo( /*AUTOINST*/);
			





   
endmodule // pull_arb
// New File: <autoinst_cavanaugh_pull.v>
module  testmux
   (
    input wire [2:0] a
    );
endmodule

module  top_test();

   /* testmux AUTO_TEMPLATE "\(.*\)$" (
    .a (@_a_value[]),
    );
    */

   testmux foo1_1
     (/*AUTOINST*/);

   testmux foo1_2
     (/*AUTOINST*/);

endmodule
// New File: <autoinst_ciu.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2012 by brad Dobbie.

module mod;
   submod #
     (.VEC_W(8),
      .IDX_W($clog2(VEC_W)))
   submod
     (/*AUTOINST*/);
endmodule

module submod (/*AUTOARG*/);
   parameter VEC_W = 32;
   parameter IDX_W = $clog2(VEC_W);
   input  [VEC_W-1:0]   vec;
   output [IDX_W-1:0]   idx;
endmodule

// Local Variables:
// verilog-auto-inst-param-value:t
// End:
// New File: <autoinst_clog2_bug522.v>
module ex;

   subwidth u_a2	// commented
     (/*AUTOINST*/
      // Outputs
      .bus4a				(bus4a[0:3]));

endmodule

module subwidth (/*AUTOARG*/
   // Outputs
   bus4a
   );
   output [0:3] bus4a;
endmodule
// New File: <autoinst_cmtcell_msg270.v>
module t;

   /*fifo_s AUTO_TEMPLATE (
    .ignored_signal         (1'b1),
    .out_signal             (NC),
    .out_bus                (out_bus[]),
    );
    */
   fifo_s data_fifo (
   //fifo_s data_fifo (
                     // Inputs
                     .clk                  (fifo_clk),
                     /*AUTOINST*/);

   /*fifo_s AUTO_TEMPLATE (
    .ignored_signal         (1'b1),
    .out_signal             (NC),
    .out_bus                (out_bus[]),
    );
    */
   //fifo_s data_fifo (
   fifo_s data_fifo (
                     // Inputs
                     .clk                  (fifo_clk),
                     /*AUTOINST*/);
endmodule

module fifo_s;
   input ignored_signal;
   input reset;
   output [31:0] out_bus;
   output 	 out_signal;
endmodule
// New File: <autoinst_cmtinst_bug383.v>
`define auto_trq #1     // clock to q delay
`define auto_tcq #1     // reset to q delay

module autoinst_crawford_array_a(/*AUTOARG*/
   // Outputs
   a_valid, a_data,
   // Inputs
   a_ready, clk, rst_n
   );

   // a interface
   output a_valid;
   output [7:0] a_data;
   input 	a_ready;

   // clock interface
   input 	clk;
   input 	rst_n;

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg [7:0]		a_data;
   reg			a_valid;
   // End of automatics

   always @(posedge clk or negedge rst_n) begin
      if(~rst_n) begin
         a_valid <= `auto_trq 0;
         a_data <= `auto_trq 0;
      end
      else begin
         if(a_ready) begin
            a_valid <= `auto_tcq 1;
            a_data <= `auto_tcq 8'hcc;
         end
      end
   end

endmodule
// New File: <autoinst_crawford_array_a.v>
`define AUTO_NBR_A_INST 2

module autoinst_crawford_array(/*AUTOARG*/
   // Outputs
   a_valid, a_data,
   // Inputs
   a_ready, clk, rst_n
   );

   // a interface
   output [`AUTO_NBR_A_INST*1-1:0] a_valid;
   output [`AUTO_NBR_A_INST*8-1:0] a_data;
   input [`AUTO_NBR_A_INST*1-1:0]  a_ready;

   // clock interface
   input 			   clk;
   input 			   rst_n;

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics

   // non-arrayed example
   autoinst_crawford_array_a auto_a_0
     (/*AUTOINST*/
      // Outputs
      .a_valid				(a_valid),
      .a_data				(a_data[7:0]),
      // Inputs
      .a_ready				(a_ready),
      .clk				(clk),
      .rst_n				(rst_n));

   autoinst_crawford_array_a auto_a_1
     (/*AUTOINST*/
      // Outputs
      .a_valid				(a_valid),
      .a_data				(a_data[7:0]),
      // Inputs
      .a_ready				(a_ready),
      .clk				(clk),
      .rst_n				(rst_n));

   // Arrayed instances
   // AUTOINST does not work for this one :-(
   // Note it is tricky because I want clk and rst_n to fanout to both instances,
   // but I want the valid signals to be discreatly tied to each submodule.

   //autoinst_crawford_array_a ary [`AUTO_NBR_A_INST-1:0]
   //  (/*XXXAUTOINST*/
   //   // Outputs
   //   .a_valid                           (a_valid[1:0]),
   //   .a_data                            (a_data[15:0]),
   //   // Inputs
   //   .a_ready                           (a_ready[1:0]),
   //   .clk                               (clk),
   //   .rst_n                             (rst_n));

   autoinst_crawford_array_a ary [`AUTO_NBR_A_INST-1:0]
     (/*AUTOINST*/
      // Outputs
      .a_valid				(a_valid),
      .a_data				(a_data[7:0]),
      // Inputs
      .a_ready				(a_ready),
      .clk				(clk),
      .rst_n				(rst_n));

   autoinst_crawford_array_a #(.a(a),b) par [`AUTO_NBR_A_INST-1:0]
     (/*AUTOINST*/
      // Outputs
      .a_valid				(a_valid),
      .a_data				(a_data[7:0]),
      // Inputs
      .a_ready				(a_ready),
      .clk				(clk),
      .rst_n				(rst_n));

endmodule
// New File: <autoinst_crawford_array.v>
// See also autoinst_defs

`define foo 6
`define bar 0

module autoinst_dedefine;

   /*AUTOOUTPUT*/
   /*AUTOWIRE*/

   /* bar AUTO_TEMPLATE (
    .\(.*\)	(@"(concat vl-name (verilog-symbol-detick-text vl-bits))"),
    ); */

   bar sub
     (/*AUTOINST*/);

endmodule

module bar;
   output onewide;
   output [3:1] fourwide;
   output [`foo-1:`bar] varwide;
endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:

// New File: <autoinst_dedefine.v>
module AA(
	  /*AUTOINPUT*/
	  /*AUTOOUTPUT*/
   input  wire clock,
   input  wire reset,
   input  wire test_enable,
   input  wire afe_ctrl,
   input  wire cpu_wr,
   input  wire [4:0] cpu_addr,
   input  wire [7:0] cpu_wdata,
   output wire [7:0] core_data,
   output wire core_code_error,
   output wire core_code_idle,
   output wire [7:0] cpu_rdata
);
endmodule

module BB(
	  /*AUTOINPUT*/
	  /*AUTOOUTPUT*/
    input wire clock,
    input wire reset,
    input wire test_enable,
    input wire core_code_idle,
    input wire core_code_error,
    input wire [7:0] core_data,
    input wire [8:0] mbist_done,
    input wire [8:0] mbist_fail,
    output wire mbist_rst,
    output reg mbist_test
);
endmodule

module TOP(
	   /*AUTOINPUT*/
	   /*AUTOOUTPUT*/
	   );

AA AA_U(/*AUTOINST*/);

BB BB_U(/*AUTOINST*/);

endmodule

// Local Variables:
// verilog-library-directories:(".")
// End:
// New File: <autoinst_ding.v>
module InstMod ( ins, outs );
    output [INWIDTH-1:0] ins;
    output [OUTWIDTH-1:0] outs;
endmodule

module test_top;

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [n*INWIDTH  +: INWIDTH] ins;		// From instName of InstMod.v
   wire [n*INWIDTH  +: INWIDTH] ins2;		// From instName2 of InstMod.v
   wire [n*INWIDTH  +: INWIDTH] ins3;		// From instName3 of InstMod.v, ..., Couldn't Merge
   wire [n*OUTWIDTH +: OUTWIDTH] outs;		// From instName of InstMod.v
   wire [n*OUTWIDTH +: OUTWIDTH] outs2;		// From instName2 of InstMod.v
   wire [4*OUTWIDTH-1 : 0] outs3;		// From instName3 of InstMod.v, ..., Couldn't Merge
   // End of automatics

   genvar i;
   generate
      for (i=0; i<4; i=i+1) begin
	 /*AUTO_LISP(setq vh-n 4)*/
	 /* InstMod AUTO_TEMPLATE
	     (.ins    (ins [n*INWIDTH  +: INWIDTH]),
	      .outs   (outs[n*OUTWIDTH +: OUTWIDTH])); */

	 InstMod instName (/*AUTOINST*/
			   // Outputs
			   .ins			(ins [n*INWIDTH  +: INWIDTH]), // Templated
			   .outs		(outs[n*OUTWIDTH +: OUTWIDTH])); // Templated

	 InstMod instName2
	     (// Inputs
	      .ins    (ins2 [n*INWIDTH  +: INWIDTH]),
	      // Outputs
	      .outs   (outs2[n*OUTWIDTH +: OUTWIDTH])
	      /*AUTOINST*/);

	 // This works but is ugly
	 InstMod instName3
	     (// Inputs
	      .ins    (ins3 [n*INWIDTH  +: INWIDTH]),
	      // Outputs
	      .outs   (outs3[n*OUTWIDTH +: OUTWIDTH])
`ifdef NEVER
	      // Inouts
	      .ins    (ins3 [4*INWIDTH-1  : 0]),
	      .outs   (outs3[4*OUTWIDTH-1 : 0])
`endif
	      /*AUTOINST*/);

      end
   endgenerate
endmodule
// New File: <autoinst_for_myers.v>
module autoinst_func;

   /*AUTO_LISP(defun testfunc (sig bits) (let ((result "{"))
    (setq result (concat result sig "[0" bits "]"))
    (concat result "}")))*/

   /* autoinst_wildcard_sub AUTO_TEMPLATE (
    .sd0_adrs@		(myport@"(substring \\"\1\\" 0 1)"),
    .\(.*\)			(@"(testfunc vl-name vl-bits)"),
    ); */

   autoinst_wildcard_sub sub0
     (/*AUTOINST*/
      // Inouts
      .sd0_adrs0			(myport0),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs1			(myport1),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs10			(myport1),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs11			(myport1),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs2			(myport2),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs3			(myport3),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs4			(myport4),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs5			(myport5),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs6			(myport6),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs7			(myport7),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs8			(myport8),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_adrs9			(myport9),		 // Templated LHS: ^sd0_adrs\([0-9]+\)$
      .sd0_ba0				({sd0_ba0[0]}),		 // Templated LHS: ^\(.*\)$
      .sd0_ba1				({sd0_ba1[0]}),		 // Templated LHS: ^\(.*\)$
      .sd0_clk				({sd0_clk[0]}),		 // Templated LHS: ^\(.*\)$
      .sd0_dqm0_l			({sd0_dqm0_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm1_l			({sd0_dqm1_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm2_l			({sd0_dqm2_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm3_l			({sd0_dqm3_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm4_l			({sd0_dqm4_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm5_l			({sd0_dqm5_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm6_l			({sd0_dqm6_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd0_dqm7_l			({sd0_dqm7_l[0]}),	 // Templated LHS: ^\(.*\)$
      .sd_ras_				({sd_ras_[0]}));		 // Templated LHS: ^\(.*\)$

endmodule

// Local Variables:
// verilog-auto-inst-template-numbers: lhs
// verilog-auto-inst-sort: t
// End:
// New File: <autoinst_func.v>
//bug284

module wrapper
 (
  /*AUTOOUTPUT*/
  /*AUTOINPUT*/
  );

 //--------------------------------------------------------------------------

 // Note input/output comments aren't needed, and multiple signals
 // per line are ok
 or u_or [31:0]
   (o[31:0], i0[31:0], i1[31:0], i2[31:0],
    // Inputs,
    i3[31:0]
    /*AUTOINST*/);

// bug676
 buf # 1 mybuf[1:0]
   (bout[1:0],
    // Inputs,
    bin[1:0]
    /*AUTOINST*/);

 //--------------------------------------------------------------------------

endmodule
// New File: <autoinst_gate.v>
module AA(
   input  wire clock,
   input  wire reset,
   input  wire test_enable,
   input  wire afe_ctrl,
   input  wire cpu_wr,
   input  wire [4:0] cpu_addr,
   input  wire [7:0] cpu_wdata,
   output wire [7:0] core_data,
   output wire core_code_error,
   output wire core_code_idle,
   output wire [7:0] cpu_rdata
);
endmodule

module TOP(
	   /*AUTOINPUT*/
	   /*AUTOOUTPUT*/
	   );

   genvar x;

AA AA_U(
	// Inputs
	.test_enable			(x),
	/*AUTOINST*/);

endmodule
// New File: <autoinst_genvar.v>
// See http://www.veripool.org/issues/show/270

interface autoinst_iface270_sub;
   logic a;
   logic b;
   modport master_mp(input a, output b);
   modport slave_mp(output a, input b);
   modport monitor (input a, input b);
endinterface
// New File: <autoinst_iface270_sub.v>
// See http://www.veripool.org/issues/show/429

module top;
   /*AUTOWIRE*/

   autoinst_iface270_sub inst_if (/*AUTOINST*/);

   ifio sub (/*AUTOINST*/);

endmodule

module ifio
  (autoinst_iface270_sub inst_if);
endmodule

// Local Variables:
// verilog-auto-inst-interfaced-ports: nil
// End:
// New File: <autoinst_iface270_top_bug429.v>
// See http://www.veripool.org/issues/show/270

module top;
   /*AUTOWIRE*/

   autoinst_iface270_sub inst_if (/*AUTOINST*/);

   ifio sub (/*AUTOINST*/);

endmodule

module ifio
  (autoinst_iface270_sub inst_if);
endmodule
// New File: <autoinst_iface270_top.v>
// bug1253

interface iface #(D  = 64) (input bit clk);
   logic clk;
endinterface: iface

module dut
    #(
      parameter D = 16,
      )
      (
       input logic clk,
       iface i_if_nopar,
       iface #(.D(D)) i_if_param1,
       iface #(.FOO(FOO), .BAR(BAR)) i_if_param2,
       iface i_if_nopar3
       );
   // Interface reference directly above ends up looking like noniface
   endmodule 

module tb_top;

      dut
	     #(/*AUTOINSTPARAM*/
	       // Parameters
	       .D			(D))
         dut
	         (/*AUTOINST*/
		  // Interfaces
		  .i_if_nopar		(i_if_nopar),
		  .i_if_param1		(i_if_param1),
		  .i_if_param2		(i_if_param2),
		  .i_if_nopar3		(i_if_nopar3),
		  // Inputs
		  .clk			(clk));
   endmodule 
// New File: <autoinst_iface_noparam.v>
module autoinst_ifdef_fredrickson_200503_sub
  (input       a,
   `ifdef TEST
   input       c,
   output wire d,
   `endif
   output wire b
   );

   assign b = a;
   `ifdef TEST
   assign d = c;
   `endif

endmodule // define_sub
// New File: <autoinst_ifdef_fredrickson_200503_sub.v>
module autoinst_ifdef_fredrickson_200503_top();

   autoinst_ifdef_fredrickson_200503_sub sub
     (/*AUTOINST*/
      // Outputs
      .d				(d),
      .b				(b),
      // Inputs
      .a				(a),
      .c				(c));

endmodule // define_top
// New File: <autoinst_ifdef_fredrickson_200503_top.v>
//bug1159

interface bus (input clk, rst_n);

  logic we;
  logic re;
  logic sel;

  //ports for master module
  modport master (input clk, rst_n, sel, output re, we);

  //ports for slave modules
  modport slave (input clk, rst_n, re, we, output sel);

endinterface

package common_pkg;
  parameter NUM_SLAVES = 2;
  // typedefs ...
endpackage

module bus_mux 
  import common_pkg::*;
  (// Interfaces
  bus.slave         bus_master,             // bus from master
  bus.master        bus_slave[NUM_SLAVES],  // bus to slave
  // Outputs
  output logic      addr_fault_strb
  );

endmodule

module top;
  import common_pkg::*;

  bus   bus_master (.clk(clk), .rst_n(rst_n));
  bus   bus_slave [NUM_SLAVES](.clk(clk), .rst_n(rst_n));

  bus_mux ibus_mux (/*AUTOINST*/);

endmodule
// New File: <autoinst_import2012.v>
//bug709

module InstModule
  (input      clk,
   svi.master svi_modport,
   svi        svi_nomodport);
endmodule // InstModule

module InstModule1 import mdp_pkg::*;
  (input      clk,
   svi.master svi_modport,
   svi        svi_nomodport);
endmodule

module top;
   InstModule instName
     (/*AUTOINST*/);

    InstModule1 instName1
     (/*AUTOINST*/);

endmodule

// Local Variables:
// verilog-library-directories:(".")
// verilog-library-extensions:(".v" ".sv")
// verilog-align-typedef-words: ("svi")
// End:
// New File: <autoinst_import.v>
module ex;

   /* autoinst_paramover_sub AUTO_TEMPLATE (
    .\(.*\)(@"vl-cell-name"_\1),
    );*/

   autoinst_paramover_sub u_a0(/*AUTOINST*/
			       // Inouts
			       .a		(u_a0_a),	 // Templated
			       .b		(u_a0_b));	 // Templated

   autoinst_paramover_sub u_a1(/*AUTOINST*/
			       // Inouts
			       .a		(u_a1_a),	 // Templated
			       .b		(u_a1_b));	 // Templated

endmodule
// New File: <autoinst_instname_all.v>
module ex;

   /* autoinst_paramover_sub AUTO_TEMPLATE (
    .a(@"vl-cell-name"_in[]),
    .b(@"(substring vl-cell-name 2)"_out[]),
    );*/

   autoinst_paramover_sub u_a0(/*AUTOINST*/
			       // Inouts
			       .a		(u_a0_in[bitsa:0]), // Templated
			       .b		(a0_out[bitsb:0])); // Templated

   autoinst_paramover_sub u_a1(/*AUTOINST*/
			       // Inouts
			       .a		(u_a1_in[bitsa:0]), // Templated
			       .b		(a1_out[bitsb:0])); // Templated

endmodule
// New File: <autoinst_instname_carlh.v>
// See bug320

interface intf1;
  logic a;
  logic b;
  modport mp1 (input a, output b);
endinterface

interface intf2 (intf1.mp1 c);
  logic e;
endinterface

module top;
  intf1 c(.*);
  intf2 f(.*);
endmodule
// New File: <autoinst_interface_bug320.v>
// See bug75

module autoinst_interface
  (/*AUTOINOUTMODULE("autoinst_interface_sub")*/
  // Beginning of automatic in/out/inouts (from specific module)
  output [7:0]		count,
  input			clk,
  input			reset,
  input			start,
  my_svi.master		my_svi_port,
  my_svi		my_svi_noport,
  my_svi		my_svi_noport_upper_decl
  // End of automatics
   );
endmodule

module autoinst_interface
  (/*AUTOINOUTCOMP("autoinst_interface_sub")*/
  // Beginning of automatic in/out/inouts (from specific module)
  output		clk,
  output		reset,
  output		start,
  input [7:0]		count,
  my_svi.master		my_svi_port,
  my_svi		my_svi_noport,
  my_svi		my_svi_noport_upper_decl
  // End of automatics
   );
endmodule

module top;
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [7:0]		count;			// From submod0 of autoinst_interface_sub.v
   my_svi		my_svi_noport;		// To/From submod0 of autoinst_interface_sub.v
   my_svi		my_svi_noport_upper_decl;// To/From submod0 of autoinst_interface_sub.v
   // End of automatics
   autoinst_interface_sub submod0 (.*);
endmodule
// New File: <autoinst_interface_star.v>
interface my_svi;
   logic enable;
   logic error;
   logic [7:0] count2;
   modport master (
                   input enable,
                   output error,
                   output count2);
endinterface

module autoinst_interface_sub
  (input wire clk,
   input wire reset,
   input wire start,
   output reg [7:0] count,
   my_svi.master my_svi_port,
   my_svi my_svi_noport,
   my_svi my_svi_noport_upper_decl
   );
endmodule
// New File: <autoinst_interface_sub.v>
// See bug75

module autoinst_interface
  (/*AUTOINOUTMODULE("autoinst_interface_sub")*/
  // Beginning of automatic in/out/inouts (from specific module)
  logic [7:0]		count,
  logic			clk,
  logic			reset,
  logic			start,
  my_svi.master		my_svi_port,
  my_svi		my_svi_noport,
  my_svi		my_svi_noport_upper_decl
  // End of automatics
   );
endmodule

module autoinst_interface
  (/*AUTOINOUTCOMP("autoinst_interface_sub")*/
  // Beginning of automatic in/out/inouts (from specific module)
  logic			clk,
  logic			reset,
  logic			start,
  logic [7:0]		count,
  my_svi.master		my_svi_port,
  my_svi		my_svi_noport,
  my_svi		my_svi_noport_upper_decl
  // End of automatics
   );
endmodule

module top;
   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic [7:0]		count;			// From submod0 of autoinst_interface_sub.v
   // End of automatics

   my_svi		my_svi_noport_upper_decl ();

   autoinst_interface_sub submod0 (/*AUTOINST*/
				   // Interfaces
				   .my_svi_port		(my_svi_port.master),
				   .my_svi_noport	(my_svi_noport),
				   .my_svi_noport_upper_decl(my_svi_noport_upper_decl),
				   // Outputs
				   .count		(count[7:0]),
				   // Inputs
				   .clk			(clk),
				   .reset		(reset),
				   .start		(start));
endmodule


// Local Variables:
// verilog-align-typedef-words: ("my_svi")
// End:
// New File: <autoinst_interface.v>
module bfm (/*AUTOARG*/
   // Inputs
   name
   );
   input [8*5:1] name ;
endmodule

module tb;
   // -------------------------------------------------------------------------
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics
   // -------------------------------------------------------------------------
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   // End of automatics
   // -------------------------------------------------------------------------
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics
   // -------------------------------------------------------------------------
   /* AUTO_CONSTANT ( "name0" "name1" "name2" ) */
   // -------------------------------------------------------------------------
   /* bfm AUTO_TEMPLATE (
    // Inputs
    .name ("name@"));
    */
   // -------------------------------------------------------------------------
   bfm bmf0 (/*AUTOINST*/
	     // Inputs
	     .name			("name0"));		 // Templated
   // -------------------------------------------------------------------------
   bfm bmf1 (/*AUTOINST*/
	     // Inputs
	     .name			("name1"));		 // Templated
   // -------------------------------------------------------------------------
   bfm bmf2 (/*AUTOINST*/
	     // Inputs
	     .name			("name2"));		 // Templated
   // -------------------------------------------------------------------------
endmodule
// New File: <autoinst_johnson.v>
module foo();
   bar i0 (
	   /*AUTOINST*/
	   );
endmodule // foo

module bar(
`line 2 "bar.v" 0
	   input  logic a,
`line 3 "bar.v" 0
	   input  logic b,
`line 4 "bar.v" 0
	   output logic c,
`line 5 "bar.v" 0
	   input  logic clk
`line 6 "bar.v" 0
	   );
endmodule // bar
// New File: <autoinst_line.v>
module autoinst_lopaz_srpad (/*AUTOARG*/
   // Outputs
   pin_in,
   // Inouts
   pin,
   // Inputs
   clk, pin_out, pin_outen
   );
   parameter w = 1;
   input     clk;

   inout [w-1:0] pin;
   output [2*w-1:0] pin_in;
   input [w-1:0]    pin_out;
   input 	    pin_outen;

endmodule
// New File: <autoinst_lopaz_srpad.v>
module io1_sub(
	       /*AUTOARG*/);

   wire	[42:0]	bscan_data;         // boundary scan stitch
   parameter 	bscan_count = 0;

   assign 	bscan_data[0] = bscan_in;

   /*
    * Emacs template to auto instaniate MD[31:0] pads
    */
   /*
    autoinst_lopaz_srpad AUTO_TEMPLATE (
    .pin(MD[@]),
    .pin_in({SDRAM_DQ_in[@],SDRAM_DQ_in[@]}),
    .pin_out(SDRAM_DQ_out[@]),
    .pin_outen(SDRAM_DQ_outen),
    .sdrmode(SDRAM_single_rate),
    .hw_enb(SDRAM_upper_word_enb),
    .ff_rptr(SDRAM_ddr_inff_sel),
    .ff_wptr(ddr_inff_enbH),
    .clk(data_strobeH),
    .bscan_so(bscan_data[@ + 1]),
    .bscan_si(bscan_data[@]),
    .bscan_shift(BScanShift),
    .bscan_clock(BScanClock),
    .bscan_mode(BScanMode),
    .bscan_update(BScanUpdate),
    .bscan_outen(SDRAM_DQ_bscan_outen),
    );
    */

   autoinst_lopaz_srpad MD31_pad (/*AUTOINST*/);


   /* autoinst_lopaz_srpad AUTO_TEMPLATE (
    .pin(MD[@"num"]),
    );
    */

   /*AUTO_LISP(setq num 1)*/
   autoinst_lopaz_srpad MD31_pad11 (/*AUTOINST*/);

   /* autoinst_lopaz_srpad AUTO_TEMPLATE (
    .pin(MD[@"num"]),
    );
    */

   /*AUTO_LISP(setq num 2)*/
   autoinst_lopaz_srpad MD31_pad11 (/*AUTOINST*/);

endmodule
// New File: <autoinst_lopaz.v>
// From: "Ma, Zhenqiang" <Zhenqiang.Ma@caviumnetworks.com>

module test (
   // Ports for module A
   input  i_A_outsidei,
   output o_A_outsideo,

   // Ports for module B
   input  i_B_outsidei,
   output o_B_outsideo );

  /*AUTOWIRE*/

   //-----------------------------------------------------------------------------
   // instantiate module A
   //-----------------------------------------------------------------------------
   
   /* moduleA AUTO_TEMPLATE (
    .[iot]_\(.*\)        (@"(vl-prefix-i-o \\"\1\\")"\1[]),
   ); */

   moduleA u0(
     /*AUTOINST*/);


  //-----------------------------------------------------------------------------
  // instantiate module B
  //-----------------------------------------------------------------------------
  
  /* moduleB AUTO_TEMPLATE (
    .[iot]_\(.*\)        (@"(vl-prefix-i-o vl-dir)"\1[]),
  ); */

  moduleB u1(
       /*AUTOINST*/);


endmodule

module moduleA (
    input  i_A_outsidei,
    output o_A_outsideo,

    input  i_B_internal,
    output o_A_internal
  );
  /*AUTOTIEOFF*/
endmodule

module moduleB (
    input  i_B_outsidei,
    output o_B_outsideo,

    input  i_A_internal,
    output o_B_internal
  );
  /*AUTOTIEOFF*/
endmodule

/*
 Local Variables:
 eval:
  (defun vl-prefix-i-o (dir)
        (cond ((equal dir "input")
               "i_")
              ((equal dir "output")
               "o_")
              ((equal dir "inout")
               "t_")
              (t "")))   
 End:
*/   
// New File: <autoinst_ma_io_prefix.v>
module top

  // no-connecting unused outputs of an interface and tying only inputs to gnd.

  // Requested:
  // 
  //  /* xx AUTO_TEMPLATE (
  //.TWI_\(.*\) @"(vl-dir (input)"    ({@"vl-width"{1'b0}}),
  //.TWI_\(.*\) @"(vl-dir (output)"    (),
  //);
  //   */

  /* xx AUTO_TEMPLATE (
   .TWI_\(.*\) (@"(if (equal vl-dir \\"output\\") \\"\\" (concat vl-width \\"'b0\\"))"),
   );
   */

   xx  XX (/*AUTOINST*/
	   // Outputs
	   .TWI_qb			(),			 // Templated
	   // Inputs
	   .clk				(clk),
	   .TWI_ia			(1'b0),			 // Templated
	   .TWI_iw			(16'b0));		 // Templated
endmodule

module xx
   (input clk,

    input  TWI_ia,
    input  [15:0] TWI_iw,
    output TWI_qb);
endmodule
// New File: <autoinst_mccoy.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2008-2008 by Wilson Snyder.

module autoinst_moddefine (/*AUTOARG*/);

   /*AUTOWIRE*/

`define SUBMOD_A submod_mod
`define SUBNAME_B  subname_b

   `SUBMOD_A `SUBNAME_B
     (/*AUTOINST*/);

   `SUBMOD_UNDEFED subundefed
     (/*AUTOINST*/);

   submod_decl_from_def subundefed
     (/*AUTOINST*/);

endmodule

module submod_mod (/*AUTOARG*/);
   input a;
   output q;
endmodule

module SUBMOD_UNDEFED (/*AUTOARG*/);
   output q;
endmodule

`define SUBMOD_DECL submod_decl_from_def
module `SUBMOD_DECL (/*AUTOARG*/);
   output q;
endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autoinst_moddefine.v>
//bug565

module InstModule
  (svi.master svi_modport,
   input      clk,
   svi        svi_nomodport);
endmodule

module InstModule1 #
  (parameter TCQ = 100)
  (svi.master svi_modport,
   input      clk,
   svi        svi_nomodport);
endmodule

module top;
   InstModule instName
     (/*AUTOINST*/
      // Interfaces
      .svi_modport			(svi_modport.master),
      .svi_nomodport			(svi_nomodport),
      // Inputs
      .clk				(clk));

    InstModule1 instName1
     (/*AUTOINST*/
      // Interfaces
      .svi_modport			(svi_modport.master),
      .svi_nomodport			(svi_nomodport),
      // Inputs
      .clk				(clk));
  
endmodule

// Local Variables:
// verilog-library-directories:(".")
// verilog-library-extensions:(".v" ".sv")
// verilog-align-typedef-words: ("svi")
// End:
// New File: <autoinst_modport_param.v>
module top;
   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic		sig1;			// From u_x of x.v
   logic [A-1:0]	sig2;			// From u_x of x.v
   logic [A-1:0] [B-1:0] sig3;			// From u_x of x.v
   logic [A-1:0][B-1:0] [C-1:0] sig4;		// From u_x of x.v
   logic [A-1:0][B-1:0][C-1:0] [D-1:0] sig5;	// From u_x of x.v
   logic		vsig1;			// From u_x of x.v
   logic		vsig2 [W-1:0];		// From u_x of x.v
   logic		vsig3 [W-1:0][X-1:0];	// From u_x of x.v
   logic		vsig4 [W-1:0][X-1:0][Y-1:0];// From u_x of x.v
   logic		vsig5 [W-1:0][X-1:0][Y-1:0][Z-1:0];// From u_x of x.v
   logic		zsig1;			// From u_x of x.v
   logic [A-1:0]	zsig2 [W-1:0];		// From u_x of x.v
   logic [A-1:0] [B-1:0] zsig3 [W-1:0][X-1:0];	// From u_x of x.v
   logic [A-1:0][B-1:0] [C-1:0] zsig4 [W-1:0][X-1:0][Y-1:0];// From u_x of x.v
   logic [A-1:0][B-1:0][C-1:0] [D-1:0] zsig5 [W-1:0][X-1:0][Y-1:0][Z-1:0];// From u_x of x.v
   // End of automatics

   x u_x (/*AUTOINST*/
	  // Outputs
	  .sig1				(sig1),
	  .sig2				(sig2[A-1:0]),
	  .sig3				(sig3/*[A-1:0][B-1:0]*/),
	  .sig4				(sig4/*[A-1:0][B-1:0][C-1:0]*/),
	  .sig5				(sig5/*[A-1:0][B-1:0][C-1:0][D-1:0]*/),
	  .vsig1			(vsig1),
	  .vsig2			(vsig2/*.[W-1:0]*/),
	  .vsig3			(vsig3/*.[W-1:0][X-1:0]*/),
	  .vsig4			(vsig4/*.[W-1:0][X-1:0][Y-1:0]*/),
	  .vsig5			(vsig5/*.[W-1:0][X-1:0][Y-1:0][Z-1:0]*/),
	  .zsig1			(zsig1),
	  .zsig2			(zsig2/*[A-1:0].[W-1:0]*/),
	  .zsig3			(zsig3/*[A-1:0][B-1:0].[W-1:0][X-1:0]*/),
	  .zsig4			(zsig4/*[A-1:0][B-1:0][C-1:0].[W-1:0][X-1:0][Y-1:0]*/),
	  .zsig5			(zsig5/*[A-1:0][B-1:0][C-1:0][D-1:0].[W-1:0][X-1:0][Y-1:0][Z-1:0]*/));
endmodule // top

module x;
   output 			       sig1;
   output [A-1:0] 		       sig2;
   output [A-1:0][B-1:0] 	       sig3;
   output [A-1:0][B-1:0][C-1:0]        sig4;
   output [A-1:0][B-1:0][C-1:0][D-1:0] sig5;

   output  vsig1;
   output  vsig2 [W-1:0];
   output  vsig3 [W-1:0][X-1:0];
   output  vsig4 [W-1:0][X-1:0][Y-1:0];
   output  vsig5 [W-1:0][X-1:0][Y-1:0][Z-1:0];

   output 			       zsig1;
   output [A-1:0] 		       zsig2 [W-1:0];
   output [A-1:0][B-1:0] 	       zsig3 [W-1:0][X-1:0];
   output [A-1:0][B-1:0][C-1:0]        zsig4 [W-1:0][X-1:0][Y-1:0];
   output [A-1:0][B-1:0][C-1:0][D-1:0] zsig5 [W-1:0][X-1:0][Y-1:0][Z-1:0];
endmodule
// New File: <autoinst_multidim.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2011 by Wilson Snyder.

// Reported by Julian Gorfajn <jig1@cornell.edu>

module autoinst_multitemplate ();

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		Boo_ba1;		// To suba1 of SubA.v
   input		Boo_ba2;		// To suba2 of SubB.v
   input		Boo_ba3;		// To suba3 of SubC.v
   input		b;			// To suba2 of SubB.v
   input		c;			// To suba3 of SubC.v
   // End of automatics

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*
     SubA AUTO_TEMPLATE "..\(...\)"
     SubB AUTO_TEMPLATE "..\(...\)"
     SubC AUTO_TEMPLATE "..\(...\)" (
     .a (Boo_@),
    );*/

   SubA suba1 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo_ba1));		 // Templated
   SubB suba2 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo_ba2),		 // Templated
	       .b			(b));
   SubC suba3 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo_ba3),		 // Templated
	       .c			(c));

endmodule

module SubA (input a);
endmodule

module SubB (input a,input b);
endmodule

module SubC (input a,input c );
endmodule
// New File: <autoinst_multitemplate_at.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2011 by Wilson Snyder.

// Reported by Julian Gorfajn <jig1@cornell.edu>

module autoinst_multitemplate ();

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		Boo1;			// To suba1 of SubA.v
   input		Boo2;			// To suba2 of SubB.v
   input		Boo3;			// To suba3 of SubC.v
   input		b;			// To suba2 of SubB.v
   input		c;			// To suba3 of SubC.v
   // End of automatics

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*
     SubA AUTO_TEMPLATE
     SubB AUTO_TEMPLATE
     SubC AUTO_TEMPLATE (
     .a (Boo@),
    );*/

   SubA suba1 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo1));			 // Templated
   SubB suba2 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo2),			 // Templated
	       .b			(b));
   SubC suba3 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo3),			 // Templated
	       .c			(c));

endmodule

module SubA (input a);
endmodule

module SubB (input a,input b);
endmodule

module SubC (input a,input c );
endmodule
// New File: <autoinst_multitemplate.v>
// issue 1698

module sub_mod
(
   input [8+4-1:0][7:0] add_left,
   input [8-4-1:0][7:0] substract_left,
   input [8*4-1:0][7:0] multiply_left,
   input [8/4-1:0][7:0] divide_left

   input [7:0][8+4-1:0] add_right,
   input [7:0][8-4-1:0] substract_right,
   input [7:0][8*4-1:0] multiply_right,
   input [7:0][8/4-1:0] divide_right,
);
endmodule : sub_mod

module top_mod
(
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input [8+4-1:0] [7:0] add_left,              // To sub_mod_i of sub_mod.v
   input [7:0] [11:0]   add_right,              // To sub_mod_i of sub_mod.v
   input [8/4-1:0] [7:0] divide_left,           // To sub_mod_i of sub_mod.v
   input [7:0] [1:0]    divide_right,           // To sub_mod_i of sub_mod.v
   input [8*4-1:0] [7:0] multiply_left,         // To sub_mod_i of sub_mod.v
   input [7:0] [31:0]   multiply_right,         // To sub_mod_i of sub_mod.v
   input [8-4-1:0] [7:0] substract_left,        // To sub_mod_i of sub_mod.v
   input [7:0] [3:0]    substract_right        // To sub_mod_i of sub_mod.v
   // End of automatics
);

   sub_mod sub_mod_i
      (/*AUTOINST*/
       // Inputs
       .add_left                        (add_left/*[8+4-1:0][7:0]*/),
       .substract_left                  (substract_left/*[8-4-1:0][7:0]*/),
       .multiply_left                   (multiply_left/*[8*4-1:0][7:0]*/),
       .divide_left                     (divide_left/*[8/4-1:0][7:0]*/),
       .add_right                       (add_right/*[7:0][8+4-1:0]*/),
       .substract_right                 (substract_right/*[7:0][8-4-1:0]*/),
       .multiply_right                  (multiply_right/*[7:0][8*4-1:0]*/),
       .divide_right                    (divide_right/*[7:0][8/4-1:0]*/));
endmodule
// New File: <autoinst_mul.v>
module autoinst_name_bug245 (
   );

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*Sub AUTO_TEMPLATE (
    .dtmp_NO (d_NO),
    .etmp_dotnamed (etmp_dotnamed),
    );*/

   Sub sub (
	    .bign1_dotnamed,
	    // Outputs
	    .bign2_dotnamed,
	    /*AUTOINST*/);

endmodule

module Sub
  (
   input  logic clk,
   output [2:0] a_NO,
   output [3:0] f4_dotnamed,
   output bign1_dotnamed,
   output [1:0] bign2_dotnamed,
   output c_dotnamed,
   output dtmp_NO,
   output etmp_dotnamed,
   input \escaped*dot*named 
   );
endmodule

// Local Variables:
// verilog-auto-inst-dot-name: t
// verilog-auto-inst-vector: nil
// End:
module autoinst_name_bug245 (
   );

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*Sub AUTO_TEMPLATE (
    .dtmp_NO (d_NO),
    .etmp_dotnamed (etmp_dotnamed),
    );*/

   Sub sub (
	    .bign1_dotnamed,
	    // Outputs
	    .bign2_dotnamed,
	    /*AUTOINST*/);

endmodule

module Sub
  (
   input  logic clk,
   output [2:0] a_NO,
   output [3:0] f4_dotnamed,
   output bign1_dotnamed,
   output [1:0] bign2_dotnamed,
   output c_dotnamed,
   output dtmp_NO,
   output etmp_dotnamed,
   input \escaped*dot*named 
   );
endmodule

// Local Variables:
// verilog-auto-inst-dot-name: t
// verilog-auto-inst-vector: nil
// End:
module autoinst_name_bug245 (
   );

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*Sub AUTO_TEMPLATE (
    .dtmp_NO (d_NO),
    .etmp_dotnamed (etmp_dotnamed),
    );*/

   Sub sub (
	    .bign1_dotnamed,
	    // Outputs
	    .bign2_dotnamed,
	    /*AUTOINST*/);

endmodule

module Sub
  (
   input  logic clk,
   output [2:0] a_NO,
   output [3:0] f4_dotnamed,
   output bign1_dotnamed,
   output [1:0] bign2_dotnamed,
   output c_dotnamed,
   output dtmp_NO,
   output etmp_dotnamed,
   input \escaped*dot*named 
   );
endmodule

// Local Variables:
// verilog-auto-inst-dot-name: t
// verilog-auto-inst-vector: nil
// End:
// New File: <autoinst_name_bug245.v>
module sub1 (/*AUTOARG*/
   // Inputs
   bus1
   );

   input [0:3] bus1;

endmodule

module sub2 (/*AUTOARG*/
   // Inputs
   bus2
   );

   input [0:3] bus2;

endmodule

module sub3 (/*AUTOARG*/
   // Outputs
   bus1, bus2
   );

   output [0:3] bus1;
   output [4:7] bus2;

endmodule


module top (/*AUTOARG*/);

   wire [4:7]		bus2;			// From sub3 of sub3.v

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [0:3]		bus1;			// From sub3 of sub3.v
   // End of automatics

   sub1 sub1 (/*AUTOINST*/
	      // Inputs
	      .bus1			(bus1[0:3]));

   sub2 sub2 (/*AUTOINST*/
	      // Inputs
	      .bus2			(bus2[0:3]));

   sub3 sub3 (/*AUTOINST*/
	      // Outputs
	      .bus1			(bus1[0:3]),
	      .bus2			(bus2));

endmodule

// Local Variables:
// verilog-auto-inst-vector:nil
// End:
// New File: <autoinst_nicholl.v>
module TEST_TOP (
		);
/*TEST AUTO_TEMPLATE
	(
 
         .abcd_efgh_ijklmno_f02_out_c(abcdefg_f02_clroilouull[5]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_be_2(abcdefg_f10_eg2_ab_cdefghijklm[49]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_fh_2(abcdefg_f08_eg0_ab_a_fghi[6]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ch(abcdefg_f09_eg1_ab_cdefghijklm[36]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ej_1(abcdefg_f10_eg2_ab_cdefghijklm[14]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ga_1(abcdefg_f10_eg2_ab_cdefghijklm[3]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fe(abcdefg_f09_eg1_ab_cdefghijklm[9]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bi_2(abcdefg_f08_eg0_ab_cdefghijklm[45]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_24_1(abcdefg_f09_eg1_ab_fghijklm[24]),
         .abcd_efgh_ijklmno_f01_oilouull_o_0_1(abcdefg_f01_oilouull[0]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_cc_2(abcdefg_f10_eg2_ab_cdefghijklm[41]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_e_2(abcdefg_f11_eg3_ab_cdefghijklm[59]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_54_1(abcdefg_f11_eg3_ab_fghijklm[54]),
         .abcd_efgh_ijklmno_f03_oilouull_o_3_1(abcdefg_f03_oilouull[3]),
         .abcd_efgh_ijklmno_f02_out_h_2(abcdefg_f02_a_zxdf[0]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_g_1(abcdefg_f11_eg3_ab_a_fghi[57]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_b_2(abcdefg_f08_eg0_ab_cdefghijklm[62]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_9_1(abcdefg_f11_eg3_ab_fghijklm[9]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_di_1(abcdefg_f09_eg1_ab_a_fghi[25]),
         .abcd_efgh_ijklmno_f00_out_h_2(abcdefg_f00_a_zxdf[0]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_cg_2(abcdefg_f11_eg3_ab_cdefghijklm[37]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_eh_2(abcdefg_f10_eg2_ab_a_fghi[16]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_di_1(abcdefg_f08_eg0_ab_cdefghijklm[25]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ec_2(abcdefg_f11_eg3_ab_cdefghijklm[21]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_d_1(abcdefg_f11_eg3_ab_a_fghi[60]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bh_1(abcdefg_f11_eg3_ab_a_fghi[46]),
         .abcd_efgh_ijklmno_f00_out_f(abcdefg_f00_clroilouull[2]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_52_1(abcdefg_f11_eg3_ab_fghijklm[52]),
         .abcd_efgh_ijklmno_f02_out_g(abcdefg_f02_clroilouull[1]),
         .abcd_efgh_ijklmno_f07_out_e(abcdefg_f07_clroilouull[3]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ff_2(abcdefg_f10_eg2_ab_a_fghi[8]),
         .abcd_efgh_ijklmno_f04_out_h(abcdefg_f04_clroilouull[0]),
         .abcd_efgh_ijklmno_f04_out_g_2(abcdefg_f04_a_zxdf[1]),
         .abcd_efgh_ijklmno_f02_out_c_2(abcdefg_f02_a_zxdf[5]),
         .abcd_efgh_ijklmno_f04_out_a_3(abcdefg_f04_a_zxdf[7]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_fa_1(abcdefg_f08_eg0_ab_cdefghijklm[13]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ed_2(abcdefg_f08_eg0_ab_a_fghi[20]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ea_2(abcdefg_f10_eg2_ab_a_fghi[23]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_c_2(abcdefg_f10_eg2_ab_cdefghijklm[61]),
         .abcd_efgh_ijklmno_f03_oilouull_o_0_1(abcdefg_f03_oilouull[0]),
         .abcd_efgh_ijklmno_f00_out_e_2(abcdefg_f00_a_zxdf[3]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bg_3(abcdefg_f10_eg2_ab_a_fghi[47]),
         .abcd_efgh_ijklmno_f05_oilouull_o_2_1(abcdefg_f05_oilouull[2]),
         .abcd_efgh_ijklmno_f01_out_h_2(abcdefg_f01_a_zxdf[0]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_44_1(abcdefg_f10_eg2_ab_fghijklm[44]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_j_3(abcdefg_f08_eg0_ab_a_fghi[54]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_39_1(abcdefg_f08_eg0_ab_fghijklm[39]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_fj_2(abcdefg_f08_eg0_ab_a_fghi[4]),
         .abcd_efgh_ijklmno_f05_out_h(abcdefg_f05_clroilouull[0]),
         .abcd_efgh_ijklmno_f05_out_d_2(abcdefg_f05_a_zxdf[4]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_gb_2(abcdefg_f10_eg2_ab_a_fghi[2]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_cb_3(abcdefg_f10_eg2_ab_a_fghi[42]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_52_1(abcdefg_f10_eg2_ab_fghijklm[52]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_be_2(abcdefg_f11_eg3_ab_cdefghijklm[49]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_42_1(abcdefg_f11_eg3_ab_fghijklm[42]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ci_1(abcdefg_f11_eg3_ab_a_fghi[35]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fh_1(abcdefg_f10_eg2_ab_cdefghijklm[6]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_24_1(abcdefg_f08_eg0_ab_fghijklm[24]),
         .abcd_efgh_ijklmno_f02_out_g_2(abcdefg_f02_a_zxdf[1]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_d_2(abcdefg_f11_eg3_ab_cdefghijklm[60]),
         .abcd_efgh_ijklmno_f06_out_d_2(abcdefg_f06_a_zxdf[4]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ea_1(abcdefg_f09_eg1_ab_a_fghi[23]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_dh_2(abcdefg_f11_eg3_ab_cdefghijklm[26]),
         .abcd_efgh_ijklmno_f04_oilouull_o_7_2(abcdefg_f04_oilouull[7]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_dh_1(abcdefg_f09_eg1_ab_a_fghi[26]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_18_1(abcdefg_f08_eg0_ab_fghijklm[18]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ba_2(abcdefg_f11_eg3_ab_cdefghijklm[53]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ce_1(abcdefg_f11_eg3_ab_a_fghi[39]),
         .abcd_efgh_ijklmno_f03_oilouull_o_5_1(abcdefg_f03_oilouull[5]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ef_1(abcdefg_f09_eg1_ab_a_fghi[18]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cj_2(abcdefg_f08_eg0_ab_cdefghijklm[34]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_j_2(abcdefg_f08_eg0_ab_cdefghijklm[54]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bh_3(abcdefg_f08_eg0_ab_a_fghi[46]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cb_1(abcdefg_f09_eg1_ab_a_fghi[42]),
         .abcd_efgh_ijklmno_f01_oilouull_o_6_2(abcdefg_f01_oilouull[6]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ba_3(abcdefg_f10_eg2_ab_a_fghi[53]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_0_1(abcdefg_f11_eg3_ab_fghijklm[0]),
         .abcd_efgh_ijklmno_f06_out_h_2(abcdefg_f06_a_zxdf[0]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_51_1(abcdefg_f08_eg0_ab_fghijklm[51]),
         .abcd_efgh_ijklmno_f06_oilouull_o_4_1(abcdefg_f06_oilouull[4]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_10_1(abcdefg_f08_eg0_ab_fghijklm[10]),
         .abcd_efgh_ijklmno_f01_oilouull_o_7_2(abcdefg_f01_oilouull[7]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_da_2(abcdefg_f11_eg3_ab_cdefghijklm[33]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_e_1(abcdefg_f09_eg1_ab_a_fghi[59]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_22_1(abcdefg_f08_eg0_ab_fghijklm[22]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_db_2(abcdefg_f11_eg3_ab_cdefghijklm[32]),
         .abcd_efgh_ijklmno_f01_oilouull_o_2_1(abcdefg_f01_oilouull[2]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ci_3(abcdefg_f08_eg0_ab_a_fghi[35]),
         .abcd_efgh_ijklmno_f07_oilouull_o_6_2(abcdefg_f07_oilouull[6]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_62_1(abcdefg_f11_eg3_ab_fghijklm[62]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_34_1(abcdefg_f10_eg2_ab_fghijklm[34]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_9_1(abcdefg_f10_eg2_ab_fghijklm[9]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_13_1(abcdefg_f10_eg2_ab_fghijklm[13]),
         .abcd_efgh_ijklmno_f05_oilouull_o_7_2(abcdefg_f05_oilouull[7]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ch_1(abcdefg_f11_eg3_ab_a_fghi[36]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fd_1(abcdefg_f10_eg2_ab_cdefghijklm[10]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fc_2(abcdefg_f10_eg2_ab_a_fghi[11]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ei_1(abcdefg_f09_eg1_ab_a_fghi[15]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_37_1(abcdefg_f08_eg0_ab_fghijklm[37]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_gb(abcdefg_f09_eg1_ab_cdefghijklm[2]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_7_1(abcdefg_f10_eg2_ab_fghijklm[7]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_dg_2(abcdefg_f11_eg3_ab_cdefghijklm[27]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ce(abcdefg_f09_eg1_ab_cdefghijklm[39]),
         .abcd_efgh_ijklmno_f07_out_d_2(abcdefg_f07_a_zxdf[4]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cd_2(abcdefg_f08_eg0_ab_cdefghijklm[40]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_57_1(abcdefg_f10_eg2_ab_fghijklm[57]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_63_1(abcdefg_f10_eg2_ab_fghijklm[63]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_j_1(abcdefg_f09_eg1_ab_a_fghi[54]),
         .abcd_efgh_ijklmno_f00_out_a(abcdefg_f00_clroilouull[7]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_46_1(abcdefg_f09_eg1_ab_fghijklm[46]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_39_1(abcdefg_f10_eg2_ab_fghijklm[39]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_28_1(abcdefg_f08_eg0_ab_fghijklm[28]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_20_1(abcdefg_f08_eg0_ab_fghijklm[20]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_51_1(abcdefg_f11_eg3_ab_fghijklm[51]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ci_1(abcdefg_f09_eg1_ab_a_fghi[35]),
         .abcd_efgh_ijklmno_f04_out_h_2(abcdefg_f04_a_zxdf[0]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_bd(abcdefg_f09_eg1_ab_cdefghijklm[50]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_dg_1(abcdefg_f10_eg2_ab_cdefghijklm[27]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_23_1(abcdefg_f09_eg1_ab_fghijklm[23]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_1_2(abcdefg_f09_eg1_ab_fghijklm[1]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bc_2(abcdefg_f08_eg0_ab_cdefghijklm[51]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bc_3(abcdefg_f10_eg2_ab_a_fghi[51]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_62_1(abcdefg_f08_eg0_ab_fghijklm[62]),
         .abcd_efgh_ijklmno_f01_out_g_2(abcdefg_f01_a_zxdf[1]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_23_1(abcdefg_f11_eg3_ab_fghijklm[23]),
         .abcd_efgh_ijklmno_f03_out_e_2(abcdefg_f03_a_zxdf[3]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_b_1(abcdefg_f09_eg1_ab_a_fghi[62]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_eh(abcdefg_f09_eg1_ab_cdefghijklm[16]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_dh_1(abcdefg_f10_eg2_ab_cdefghijklm[26]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_34_1(abcdefg_f09_eg1_ab_fghijklm[34]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_gc_1(abcdefg_f10_eg2_ab_cdefghijklm[1]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cg_2(abcdefg_f08_eg0_ab_cdefghijklm[37]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_13_1(abcdefg_f11_eg3_ab_fghijklm[13]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_2_1(abcdefg_f08_eg0_ab_fghijklm[2]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fb(abcdefg_f09_eg1_ab_cdefghijklm[12]),
         .abcd_efgh_ijklmno_f00_oilouull_o_6_2(abcdefg_f00_oilouull[6]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_h_3(abcdefg_f08_eg0_ab_a_fghi[56]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_38_1(abcdefg_f09_eg1_ab_fghijklm[38]),
         .abcd_efgh_ijklmno_f00_out_c(abcdefg_f00_clroilouull[5]),
         .abcd_efgh_ijklmno_f06_out_a_3(abcdefg_f06_a_zxdf[7]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_60_1(abcdefg_f09_eg1_ab_fghijklm[60]),
         .abcd_efgh_ijklmno_f06_oilouull_o_2_1(abcdefg_f06_oilouull[2]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_8_1(abcdefg_f09_eg1_ab_fghijklm[8]),
         .abcd_efgh_ijklmno_f03_out_f(abcdefg_f03_clroilouull[2]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_dj_1(abcdefg_f08_eg0_ab_cdefghijklm[24]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bg_2(abcdefg_f08_eg0_ab_cdefghijklm[47]),
         .abcd_efgh_ijklmno_f01_oilouull_o_4_1(abcdefg_f01_oilouull[4]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ef_2(abcdefg_f10_eg2_ab_a_fghi[18]),
         .abcd_efgh_ijklmno_f01_out_a_3(abcdefg_f01_a_zxdf[7]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_12_1(abcdefg_f08_eg0_ab_fghijklm[12]),
         .abcd_efgh_ijklmno_f07_out_c_2(abcdefg_f07_a_zxdf[5]),
         .abcd_efgh_ijklmno_f00_out_e(abcdefg_f00_clroilouull[3]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ca_1(abcdefg_f09_eg1_ab_a_fghi[43]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_eg_2(abcdefg_f08_eg0_ab_a_fghi[17]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_be_1(abcdefg_f11_eg3_ab_a_fghi[49]),
         .abcd_efgh_ijklmno_f06_out_d(abcdefg_f06_clroilouull[4]),
         .abcd_efgh_ijklmno_f00_out_g(abcdefg_f00_clroilouull[1]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_b(abcdefg_f09_eg1_ab_cdefghijklm[62]),
         .abcd_efgh_ijklmno_f00_out_f_2(abcdefg_f00_a_zxdf[2]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_gc_1(abcdefg_f09_eg1_ab_a_fghi[1]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ca_1(abcdefg_f11_eg3_ab_a_fghi[43]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ea(abcdefg_f09_eg1_ab_cdefghijklm[23]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_12_1(abcdefg_f10_eg2_ab_fghijklm[12]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_gd_2(abcdefg_f11_eg3_ab_cdefghijklm[0]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_i_1(abcdefg_f09_eg1_ab_a_fghi[55]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_e_2(abcdefg_f08_eg0_ab_cdefghijklm[59]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ff(abcdefg_f09_eg1_ab_cdefghijklm[8]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_17_1(abcdefg_f11_eg3_ab_fghijklm[17]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_be_3(abcdefg_f10_eg2_ab_a_fghi[49]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_20_1(abcdefg_f11_eg3_ab_fghijklm[20]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bc_1(abcdefg_f11_eg3_ab_a_fghi[51]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_43_1(abcdefg_f09_eg1_ab_fghijklm[43]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_54_1(abcdefg_f09_eg1_ab_fghijklm[54]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_be_2(abcdefg_f08_eg0_ab_cdefghijklm[49]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_36_1(abcdefg_f10_eg2_ab_fghijklm[36]),
         .abcd_efgh_ijklmno_f05_out_h_2(abcdefg_f05_a_zxdf[0]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cc(abcdefg_f09_eg1_ab_cdefghijklm[41]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_3_1(abcdefg_f11_eg3_ab_fghijklm[3]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ff_1(abcdefg_f11_eg3_ab_a_fghi[8]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_19_2(abcdefg_f11_eg3_ab_fghijklm[19]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bi_1(abcdefg_f11_eg3_ab_a_fghi[45]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_49_1(abcdefg_f08_eg0_ab_fghijklm[49]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ee_1(abcdefg_f10_eg2_ab_cdefghijklm[19]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_29_1(abcdefg_f08_eg0_ab_fghijklm[29]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_29_1(abcdefg_f09_eg1_ab_fghijklm[29]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ed(abcdefg_f09_eg1_ab_cdefghijklm[20]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_de(abcdefg_f09_eg1_ab_cdefghijklm[29]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_dg_2(abcdefg_f10_eg2_ab_a_fghi[27]),
         .abcd_efgh_ijklmno_f04_oilouull_o_2_1(abcdefg_f04_oilouull[2]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_eb_2(abcdefg_f10_eg2_ab_a_fghi[22]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ea_2(abcdefg_f11_eg3_ab_cdefghijklm[23]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fc_1(abcdefg_f10_eg2_ab_cdefghijklm[11]),
         .abcd_efgh_ijklmno_f06_out_g(abcdefg_f06_clroilouull[1]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_61_1(abcdefg_f09_eg1_ab_fghijklm[61]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ee_2(abcdefg_f10_eg2_ab_a_fghi[19]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_47_1(abcdefg_f11_eg3_ab_fghijklm[47]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_36_1(abcdefg_f08_eg0_ab_fghijklm[36]),
         .abcd_efgh_ijklmno_f07_oilouull_o_0_1(abcdefg_f07_oilouull[0]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_j_1(abcdefg_f11_eg3_ab_a_fghi[54]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_52_1(abcdefg_f08_eg0_ab_fghijklm[52]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_gd_1(abcdefg_f11_eg3_ab_a_fghi[0]),
         .abcd_efgh_ijklmno_f07_out_h_2(abcdefg_f07_a_zxdf[0]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cg_1(abcdefg_f09_eg1_ab_a_fghi[37]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_8_1(abcdefg_f10_eg2_ab_fghijklm[8]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fh_1(abcdefg_f11_eg3_ab_a_fghi[6]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_52_1(abcdefg_f09_eg1_ab_fghijklm[52]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ci_2(abcdefg_f11_eg3_ab_cdefghijklm[35]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_15_1(abcdefg_f10_eg2_ab_fghijklm[15]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_df(abcdefg_f09_eg1_ab_cdefghijklm[28]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ee(abcdefg_f09_eg1_ab_cdefghijklm[19]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bd_1(abcdefg_f11_eg3_ab_a_fghi[50]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fe_1(abcdefg_f11_eg3_ab_a_fghi[9]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_di_2(abcdefg_f08_eg0_ab_a_fghi[25]),
         .abcd_efgh_ijklmno_f00_out_d(abcdefg_f00_clroilouull[4]),
         .abcd_efgh_ijklmno_f07_oilouull_o_5_1(abcdefg_f07_oilouull[5]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_41_1(abcdefg_f09_eg1_ab_fghijklm[41]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cf_2(abcdefg_f08_eg0_ab_cdefghijklm[38]),
         .abcd_efgh_ijklmno_f05_out_b_3(abcdefg_f05_a_zxdf[6]),
         .abcd_efgh_ijklmno_f03_oilouull_o_1_1(abcdefg_f03_oilouull[1]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_dg_1(abcdefg_f09_eg1_ab_a_fghi[27]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_eg(abcdefg_f09_eg1_ab_cdefghijklm[17]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ce_1(abcdefg_f09_eg1_ab_a_fghi[39]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fi_2(abcdefg_f11_eg3_ab_cdefghijklm[5]),
         .abcd_efgh_ijklmno_f05_out_g(abcdefg_f05_clroilouull[1]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bj_2(abcdefg_f10_eg2_ab_cdefghijklm[44]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_db(abcdefg_f09_eg1_ab_cdefghijklm[32]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_g_1(abcdefg_f09_eg1_ab_a_fghi[57]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bf_2(abcdefg_f08_eg0_ab_cdefghijklm[48]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bh_2(abcdefg_f08_eg0_ab_cdefghijklm[46]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_23_1(abcdefg_f10_eg2_ab_fghijklm[23]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_61_1(abcdefg_f10_eg2_ab_fghijklm[61]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_db_1(abcdefg_f09_eg1_ab_a_fghi[32]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_f(abcdefg_f09_eg1_ab_cdefghijklm[58]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_57_1(abcdefg_f09_eg1_ab_fghijklm[57]),
         .abcd_efgh_ijklmno_f01_oilouull_o_1_1(abcdefg_f01_oilouull[1]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_eh_2(abcdefg_f08_eg0_ab_a_fghi[16]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_41_1(abcdefg_f11_eg3_ab_fghijklm[41]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_c_2(abcdefg_f08_eg0_ab_cdefghijklm[61]),
         .abcd_efgh_ijklmno_f06_out_e(abcdefg_f06_clroilouull[3]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ej_1(abcdefg_f11_eg3_ab_a_fghi[14]),
         .abcd_efgh_ijklmno_f02_oilouull_o_1_1(abcdefg_f02_oilouull[1]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bd_2(abcdefg_f11_eg3_ab_cdefghijklm[50]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_6_1(abcdefg_f08_eg0_ab_fghijklm[6]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ee_1(abcdefg_f09_eg1_ab_a_fghi[19]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fc_2(abcdefg_f11_eg3_ab_cdefghijklm[11]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bd_3(abcdefg_f10_eg2_ab_a_fghi[50]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_df_1(abcdefg_f08_eg0_ab_cdefghijklm[28]),
         .abcd_efgh_ijklmno_f03_out_a(abcdefg_f03_clroilouull[7]),
         .abcd_efgh_ijklmno_f02_oilouull_o_0_1(abcdefg_f02_oilouull[0]),
         .abcd_efgh_ijklmno_f00_out_g_2(abcdefg_f00_a_zxdf[1]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_43_1(abcdefg_f08_eg0_ab_fghijklm[43]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_59_1(abcdefg_f08_eg0_ab_fghijklm[59]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ff_1(abcdefg_f09_eg1_ab_a_fghi[8]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bh_2(abcdefg_f10_eg2_ab_cdefghijklm[46]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_0_1(abcdefg_f09_eg1_ab_fghijklm[0]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ec_2(abcdefg_f08_eg0_ab_a_fghi[21]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_33_1(abcdefg_f09_eg1_ab_fghijklm[33]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fh_1(abcdefg_f09_eg1_ab_a_fghi[6]),
         .abcd_efgh_ijklmno_f02_oilouull_o_4_1(abcdefg_f02_oilouull[4]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_31_1(abcdefg_f10_eg2_ab_fghijklm[31]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fa_1(abcdefg_f10_eg2_ab_cdefghijklm[13]),
         .abcd_efgh_ijklmno_f05_out_f(abcdefg_f05_clroilouull[2]),
         .abcd_efgh_ijklmno_f04_out_e_2(abcdefg_f04_a_zxdf[3]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_dd_1(abcdefg_f09_eg1_ab_a_fghi[30]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_12_1(abcdefg_f11_eg3_ab_fghijklm[12]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_bf_1(abcdefg_f09_eg1_ab_a_fghi[48]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_23_1(abcdefg_f08_eg0_ab_fghijklm[23]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_dj_2(abcdefg_f08_eg0_ab_a_fghi[24]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ci_3(abcdefg_f10_eg2_ab_a_fghi[35]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_17_1(abcdefg_f08_eg0_ab_fghijklm[17]),
         .abcd_efgh_ijklmno_f02_out_f_2(abcdefg_f02_a_zxdf[2]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bb_1(abcdefg_f11_eg3_ab_a_fghi[52]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_cc_1(abcdefg_f11_eg3_ab_a_fghi[41]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_f_1(abcdefg_f11_eg3_ab_a_fghi[58]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fi(abcdefg_f09_eg1_ab_cdefghijklm[5]),
         .abcd_efgh_ijklmno_f03_out_h(abcdefg_f03_clroilouull[0]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_e_1(abcdefg_f11_eg3_ab_a_fghi[59]),
         .abcd_efgh_ijklmno_f01_out_b_3(abcdefg_f01_a_zxdf[6]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_55_1(abcdefg_f08_eg0_ab_fghijklm[55]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bj_3(abcdefg_f10_eg2_ab_a_fghi[44]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_d_2(abcdefg_f08_eg0_ab_cdefghijklm[60]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bi_3(abcdefg_f08_eg0_ab_a_fghi[45]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fg_1(abcdefg_f09_eg1_ab_a_fghi[7]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ej_2(abcdefg_f10_eg2_ab_a_fghi[14]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cb_2(abcdefg_f08_eg0_ab_cdefghijklm[42]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fi_1(abcdefg_f10_eg2_ab_cdefghijklm[5]),
         .abcd_efgh_ijklmno_f01_out_c(abcdefg_f01_clroilouull[5]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fa_2(abcdefg_f11_eg3_ab_cdefghijklm[13]),
         .abcd_efgh_ijklmno_f07_out_b_3(abcdefg_f07_a_zxdf[6]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_2_1(abcdefg_f09_eg1_ab_fghijklm[2]),
         .abcd_efgh_ijklmno_f07_oilouull_o_2_1(abcdefg_f07_oilouull[2]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ed_2(abcdefg_f10_eg2_ab_a_fghi[20]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_32_1(abcdefg_f08_eg0_ab_fghijklm[32]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_29_1(abcdefg_f10_eg2_ab_fghijklm[29]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ef_1(abcdefg_f11_eg3_ab_a_fghi[18]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_15_1(abcdefg_f09_eg1_ab_fghijklm[15]),
         .abcd_efgh_ijklmno_f04_out_c_2(abcdefg_f04_a_zxdf[5]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cd(abcdefg_f09_eg1_ab_cdefghijklm[40]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_57_1(abcdefg_f08_eg0_ab_fghijklm[57]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ee_2(abcdefg_f11_eg3_ab_cdefghijklm[19]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_3_1(abcdefg_f09_eg1_ab_fghijklm[3]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_20_1(abcdefg_f10_eg2_ab_fghijklm[20]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_16_1(abcdefg_f08_eg0_ab_fghijklm[16]),
         .abcd_efgh_ijklmno_f06_oilouull_o_1_1(abcdefg_f06_oilouull[1]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cg(abcdefg_f09_eg1_ab_cdefghijklm[37]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_cd_3(abcdefg_f10_eg2_ab_a_fghi[40]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_35_1(abcdefg_f10_eg2_ab_fghijklm[35]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_j_3(abcdefg_f10_eg2_ab_a_fghi[54]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_dc_2(abcdefg_f10_eg2_ab_a_fghi[31]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_34_1(abcdefg_f08_eg0_ab_fghijklm[34]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bi_2(abcdefg_f10_eg2_ab_cdefghijklm[45]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_48_1(abcdefg_f09_eg1_ab_fghijklm[48]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_1_2(abcdefg_f10_eg2_ab_fghijklm[1]),
         .abcd_efgh_ijklmno_f07_out_g(abcdefg_f07_clroilouull[1]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_c_2(abcdefg_f11_eg3_ab_cdefghijklm[61]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_24_1(abcdefg_f10_eg2_ab_fghijklm[24]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fj_2(abcdefg_f10_eg2_ab_a_fghi[4]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_bi_1(abcdefg_f09_eg1_ab_a_fghi[45]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_eb_2(abcdefg_f11_eg3_ab_cdefghijklm[22]),
         .abcd_efgh_ijklmno_f00_oilouull_o_5_1(abcdefg_f00_oilouull[5]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_30_1(abcdefg_f08_eg0_ab_fghijklm[30]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_11_1(abcdefg_f11_eg3_ab_fghijklm[11]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_26_1(abcdefg_f11_eg3_ab_fghijklm[26]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_45_1(abcdefg_f08_eg0_ab_fghijklm[45]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_dj_1(abcdefg_f10_eg2_ab_cdefghijklm[24]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_gd_1(abcdefg_f10_eg2_ab_cdefghijklm[0]),
         .abcd_efgh_ijklmno_f05_out_b(abcdefg_f05_clroilouull[6]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ef_2(abcdefg_f08_eg0_ab_a_fghi[18]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_dg(abcdefg_f09_eg1_ab_cdefghijklm[27]),
         .abcd_efgh_ijklmno_f02_oilouull_o_6_2(abcdefg_f02_oilouull[6]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_39_1(abcdefg_f11_eg3_ab_fghijklm[39]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_bh_3(abcdefg_f10_eg2_ab_a_fghi[46]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_fc_1(abcdefg_f08_eg0_ab_cdefghijklm[11]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fa(abcdefg_f09_eg1_ab_cdefghijklm[13]),
         .abcd_efgh_ijklmno_f04_out_b_3(abcdefg_f04_a_zxdf[6]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_21_1(abcdefg_f08_eg0_ab_fghijklm[21]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_bc_3(abcdefg_f08_eg0_ab_a_fghi[51]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_dd_1(abcdefg_f08_eg0_ab_cdefghijklm[30]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_fc_1(abcdefg_f09_eg1_ab_a_fghi[11]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_22_1(abcdefg_f11_eg3_ab_fghijklm[22]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_14_1(abcdefg_f11_eg3_ab_fghijklm[14]),
         .abcd_efgh_ijklmno_f02_out_d_2(abcdefg_f02_a_zxdf[4]),
         .abcd_efgh_ijklmno_f06_out_h(abcdefg_f06_clroilouull[0]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_37_1(abcdefg_f09_eg1_ab_fghijklm[37]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_eh_1(abcdefg_f09_eg1_ab_a_fghi[16]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_12_1(abcdefg_f09_eg1_ab_fghijklm[12]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_cd_1(abcdefg_f09_eg1_ab_a_fghi[40]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_60_1(abcdefg_f11_eg3_ab_fghijklm[60]),
         .abcd_efgh_ijklmno_f03_out_f_2(abcdefg_f03_a_zxdf[2]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_43_1(abcdefg_f11_eg3_ab_fghijklm[43]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_dc_2(abcdefg_f08_eg0_ab_a_fghi[31]),
         .abcd_efgh_ijklmno_f07_out_b(abcdefg_f07_clroilouull[6]),
         .abcd_efgh_ijklmno_f03_out_b_3(abcdefg_f03_a_zxdf[6]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ea_1(abcdefg_f10_eg2_ab_cdefghijklm[23]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_eg_1(abcdefg_f08_eg0_ab_cdefghijklm[17]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_fc_2(abcdefg_f08_eg0_ab_a_fghi[11]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_gc_1(abcdefg_f08_eg0_ab_cdefghijklm[1]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_da(abcdefg_f09_eg1_ab_cdefghijklm[33]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_gb_2(abcdefg_f11_eg3_ab_cdefghijklm[2]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_ba_2(abcdefg_f10_eg2_ab_cdefghijklm[53]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fb_2(abcdefg_f10_eg2_ab_a_fghi[12]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_ee_1(abcdefg_f11_eg3_ab_a_fghi[19]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_32_1(abcdefg_f09_eg1_ab_fghijklm[32]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_gc_2(abcdefg_f11_eg3_ab_cdefghijklm[1]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ee_1(abcdefg_f08_eg0_ab_cdefghijklm[19]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_36_1(abcdefg_f09_eg1_ab_fghijklm[36]),
         .abcd_efgh_ijklmno_f02_out_a(abcdefg_f02_clroilouull[7]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_9_1(abcdefg_f08_eg0_ab_fghijklm[9]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_5_1(abcdefg_f09_eg1_ab_fghijklm[5]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_bh_2(abcdefg_f11_eg3_ab_cdefghijklm[46]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_35_1(abcdefg_f09_eg1_ab_fghijklm[35]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ca(abcdefg_f09_eg1_ab_cdefghijklm[43]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_47_1(abcdefg_f08_eg0_ab_fghijklm[47]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_gc_1(abcdefg_f11_eg3_ab_a_fghi[1]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ba_2(abcdefg_f08_eg0_ab_cdefghijklm[53]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_44_1(abcdefg_f11_eg3_ab_fghijklm[44]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_9_1(abcdefg_f09_eg1_ab_fghijklm[9]),
         .abcd_efgh_ijklmno_f00_oilouull_o_3_1(abcdefg_f00_oilouull[3]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fe_2(abcdefg_f11_eg3_ab_cdefghijklm[9]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_ec_1(abcdefg_f09_eg1_ab_a_fghi[21]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_de_2(abcdefg_f10_eg2_ab_a_fghi[29]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_29_1(abcdefg_f11_eg3_ab_fghijklm[29]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_g_3(abcdefg_f08_eg0_ab_a_fghi[57]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fi_2(abcdefg_f10_eg2_ab_a_fghi[5]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_27_1(abcdefg_f08_eg0_ab_fghijklm[27]),
         .abcd_efgh_ijklmno_f11_eg3_ge_out_fc_1(abcdefg_f11_eg3_ab_a_fghi[11]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_42_1(abcdefg_f10_eg2_ab_fghijklm[42]),
         .abcd_efgh_ijklmno_f04_oilouull_o_4_1(abcdefg_f04_oilouull[4]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_30_1(abcdefg_f09_eg1_ab_fghijklm[30]),
         .abcd_efgh_ijklmno_f01_oilouull_o_3_1(abcdefg_f01_oilouull[3]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_fj_1(abcdefg_f10_eg2_ab_cdefghijklm[4]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_55_1(abcdefg_f11_eg3_ab_fghijklm[55]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_42_1(abcdefg_f08_eg0_ab_fghijklm[42]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_gd(abcdefg_f09_eg1_ab_cdefghijklm[0]),
         .abcd_efgh_ijklmno_f09_eg1_ge_out_eg_1(abcdefg_f09_eg1_ab_a_fghi[17]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_8_1(abcdefg_f08_eg0_ab_fghijklm[8]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_50_1(abcdefg_f11_eg3_ab_fghijklm[50]),
         .abcd_efgh_ijklmno_f06_out_f_2(abcdefg_f06_a_zxdf[2]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_26_1(abcdefg_f09_eg1_ab_fghijklm[26]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_30_1(abcdefg_f11_eg3_ab_fghijklm[30]),
         .abcd_efgh_ijklmno_f10_eg2_ab_fghijklm_o_55_1(abcdefg_f10_eg2_ab_fghijklm[55]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_df_2(abcdefg_f08_eg0_ab_a_fghi[28]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_28_1(abcdefg_f09_eg1_ab_fghijklm[28]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_22_1(abcdefg_f09_eg1_ab_fghijklm[22]),
         .abcd_efgh_ijklmno_f09_eg1_ab_fghijklm_o_45_1(abcdefg_f09_eg1_ab_fghijklm[45]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_46_1(abcdefg_f08_eg0_ab_fghijklm[46]),
         .abcd_efgh_ijklmno_f01_out_f(abcdefg_f01_clroilouull[2]),
         .abcd_efgh_ijklmno_f11_eg3_ab_fghijklm_o_36_1(abcdefg_f11_eg3_ab_fghijklm[36]),
         .abcd_efgh_ijklmno_f05_out_f_2(abcdefg_f05_a_zxdf[2]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_ch_2(abcdefg_f08_eg0_ab_cdefghijklm[36]),
         .abcd_efgh_ijklmno_f10_eg2_ge_out_cj_3(abcdefg_f10_eg2_ab_a_fghi[34]),
         .abcd_efgh_ijklmno_f04_oilouull_o_3_1(abcdefg_f04_oilouull[3]),
         .abcd_efgh_ijklmno_f08_eg0_ge_out_cd_3(abcdefg_f08_eg0_ab_a_fghi[40]),
         .abcd_efgh_ijklmno_f08_eg0_ab_fghijklm_o_56_1(abcdefg_f08_eg0_ab_fghijklm[56]),
 

		);*/
TEST TEST (/*AUTOINST*/);

endmodule

module TEST (/*AUTOARG*/);
   parameter NO = 6456;
endmodule
// New File: <autoinst_overflow_bug250.v>
// bug981

module a();
	 parameter AUM=80;
	 parameter BUM=70;
	 parameter VUM=1;
         parameter V2=2;
	 input   		    my_data_z;
	 input   		    my_data_v[VUM];
         input                      my_data_vv[VUM][V2];
	 input [AUM-1:0]  	    my_data_av[VUM];
	 input [AUM-1:0][BUM-1:0]   my_data_ab;
	 input [AUM-1:0][BUM-1:0]   my_data_abv[VUM];

	 input [XUM-1:0][YUM-1:0]   my_data_xyz[ZUM];

	 input PARAMS0__t params0 [1:0];
	 input PARAMS1__t params1 [1:0];
endmodule

module top (/*AUTOARG*/);
  /*AUTOINPUT*/
  /*AUTOOUTPUT*/
  /*AUTOWIRE*/

  /*
   a AUTO_TEMPLATE
   (
   .\(.*\) (TEST@_\1[][]),
   );
   */
  a #(/*AUTOINSTPARAM*/)
   a_0 (/*AUTOINST*/);


  a #(/*AUTOINSTPARAM*/)
   a_1 (/*AUTOINST*/);

endmodule

// Local Variables:
// verilog-auto-inst-param-value:t
// verilog-typedef-regexp: "_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <autoinst_param_2d.v>
module autoinstparam_belkind_leaf (/*AUTOARG*/) ;

   parameter P = 3'd4;
   input [P-1:0] a;

endmodule // leaf
// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autoinstparam_belkind_leaf.v>
module Ptest #(
       parameter I_CONTROL     = 8'h 00, R_CONTROL     = 8'h00)
  ( 
   input scanTest,
   input scanArst);
endmodule 

module t;

  Ptest
    #(/*AUTOINSTPARAM*/)
   u_Ptest
     (/*AUTOINST*/);

endmodule
// New File: <autoinstparam_bug287.v>
module InstMod ( ins, outs );
    output [INWIDTH-1:0] ins;
endmodule

module test (/*AUTOARG*/) ;
  parameter foo=1;
  /*
  parameter foo=2;
  */                                                                                                                                           

   //bug647

   /* InstMod AUTO_TEMPLATE
    (.ins    (a@"vh-foo")); */

   InstMod instName (/*AUTOINST*/
		     // Outputs
		     .ins		(a1));			 // Templated

endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autoinst_param_cmt.v>
module autoinstparam_first_sub (/*AUTOARG*/
   // Inouts
   a, b
   );

   //======================================================================
   // Inputs/Outputs
   //======================================================================

   localparam IGNORED;
   parameter BITSA;
   parameter type BITSB_t = bit;   // See bug340

   inout [BITSA:0] a;
   inout BITSB_t b;

endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <autoinstparam_first_sub.v>
module autoinstparam_first ();

   parameter BITSCHANGED;
   parameter BITSA;
   parameter type BITSB_t;
   typedef reg [2:0] my_bitsb_t;

   /* autoinstparam_first_sub AUTO_TEMPLATE (
       .BITSA		(BITSCHANGED),
    ); */

   autoinstparam_first_sub
     #(/*AUTOINSTPARAM*/)
       sub
	 (/*AUTOINST*/);

   autoinstparam_first_sub
     #(
       .BITSB_t				(my_bitsb_t),
       /*AUTOINSTPARAM*/)
       sub1
	 (/*AUTOINST*/);

   autoinstparam_first_sub
     #(
       .BITSA				(1),
       .BITSB_t				(my_bitsb_t)
       /*AUTOINSTPARAM*/)
       sub2
	 (/*AUTOINST*/);

   autoinstparam_first_sub
     #(
       /*AUTOINSTPARAM*/)
       sub3
	 (/*AUTOINST*/);

endmodule

// Local Variables:
// verilog-auto-inst-param-value:nil
// verilog-typedef-regexp: "_t$"
// End:
// New File: <autoinstparam_first.v>
module top;
   /*AUTOWIRE*/
   sub0 #(/*AUTOINSTPARAM*/)
   s0 (/*AUTOINST*/);
endmodule

module sub0
  #(
    parameter string testit2 = 0,
    int TESTIT = 0
    ) (
       // clk and resets
    input  logic   side_clk,
    input  logic   side_rst_b,
       );

endmodule
// New File: <autoinstparam_iface_bruce.v>
module xyz
  #(parameter int FOO=1, BAR=2,
    parameter logic [5:0] BLUP=3, ZOT=4,
    parameter LDWRDS=5)
   ( input x1, x2,
     input int i1, i2,
     input logic [5:0] i3, i4,
     input i5,
     output y); 
endmodule 

module pdq;
   input x; output y;
   parameter int FOO=1;
endmodule 

module abc; 
   xyz XYZ
     #(/*AUTOINSTPARAM*/)
   (/*AUTOINST*/); 
   pdq PDQ
     #(/*AUTOINSTPARAM*/)
   (/*AUTOINST*/); 
endmodule 
// New File: <autoinstparam_int.v>
//bug889

module leaf #(
	      parameter   DATA_WIDTH = 256,
	      localparam  STRB_WIDTH_SHOULD_BE_HIDDEN = DATA_WIDTH/8 )
   ();
endmodule

module test;
   parameter integer DATA_WIDTH = 256;
   parameter integer STRB_WIDTH = DATA_WIDTH/8;

   /* leaf AUTO_TEMPLATE (
    .DATA_WIDTH( DATA_WIDTH ),
    .STRB_WIDTH_SHOULD_BE_HIDDEN ( STRB_WIDTH ),
    .\(.*\) ( \1 ),
    );*/

   leaf
     #( /*AUTOINSTPARAM*/)
   u_leaf
     ( /*AUTOINST*/);

endmodule
// New File: <autoinstparam_local.v>
module autoinst_paramover_sub (/*AUTOARG*/
   // Inouts
   a, b
   );

   //======================================================================
   // Inputs/Outputs
   //======================================================================

   parameter bitsa;
   parameter bitsb;

   inout [bitsa:0] a;         // SDRAM Channel 0 Row Address Strobe
   inout [bitsb:0] b;         // SDRAM Channel 0 Row Address Strobe

endmodule
// New File: <autoinst_paramover_sub.v>
module autoinst_paramover (/*AUTOARG*/
   // Inouts
   a, b
   );

   //======================================================================
   // Inputs/Outputs
   //======================================================================

   parameter bitsa = 20;
   parameter bitsb = 20;

   inout [20:0] a;         // SDRAM Channel 0 Row Address Strobe
   inout [20:0] b;         // SDRAM Channel 0 Row Address Strobe

   autoinst_paramover_sub #(bitsa,bitsb ) mod
     (/*AUTOINST*/
      // Inouts
      .a				(a[bitsa:0]),
      .b				(b[bitsb:0]));

endmodule
// New File: <autoinst_paramover.v>
typedef struct packed {
   logic       size;
} config_t;

parameter config_t CFG8 = '{size: 8};
parameter config_t CFG4 = '{size: 4};

module m0 (
           /*AUTOINPUT*/
           );
   m4
     m4(/*AUTOINST*/);
   m8 #(.CFG(CFG8))
     m8(/*AUTOINST*/);
endmodule

module m4
  #(
    parameter config_t CFG = CFG4
    )
   (
    input                a4,
    input [CFG.size-1:0] b4
    );
endmodule

module m8
  #(
    parameter config_t CFG = CFG8
    )
   (
    input                a8,
    input [CFG.size-1:0] b8
    );
endmodule

// Local Variables:
// verilog-typedef-regexp: "_[tT]$"
// verilog-auto-inst-param-value:t
// verilog-auto-inst-param-value-type:t
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_[tT]\\>"
// End:
// New File: <autoinst_param_structsel.v>

//bug1061

//typedef logic [7:0] foo_t;
module ptype
  (
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input foo_t		a,			// To b0 of ptype_buf.v, ...
   // End of automatics

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output foo_t		y0,			// From b0 of ptype_buf.v
   output logic [7:0]	y1,			// From b1 of ptype_buf.v
   output TYPE_T	y2			// From b2 of ptype_buf.v
   // End of automatics
   );

   ptype_buf #(.TYPE_T(foo_t)) b0
     (// Outputs
      .y				(y0),
      /*AUTOINST*/
      // Inputs
      .a				(a));

   ptype_buf #(.TYPE_T(logic [7:0])) b1
     (// Outputs
      .y				(y1),
      /*AUTOINST*/
      // Inputs
      .a				(a));

   ptype_buf #(.WIDTH(8)) b2
     (// Outputs
      .y				(y2),
      /*AUTOINST*/
      // Inputs
      .a				(a));

endmodule

module ptype_buf
  #(parameter WIDTH = 1,
    parameter type TYPE_T = logic [WIDTH-1:0])
   (output TYPE_T y,
    input  TYPE_T a);
   assign y = a;
endmodule

///--------------

// Example in docs

	module InstModule (o,i);
	   parameter WIDTH;
	   input [WIDTH-1:0] i;
           parameter type OUT_t;
	   output OUT_t o;
	endmodule

	module ExampInst;
           /*AUTOINPUT*/    
	   // Beginning of automatic inputs (from unused autoinst inputs)
	   input [9:0]	   i;			   // To instName of InstModule.v
	   // End of automatics
         
           /*AUTOOUTPUT*/    
	   // Beginning of automatic outputs (from unused autoinst outputs)
	   output upper_t  o;			   // From instName of InstModule.v
	   // End of automatics

	   InstModule
	     #(.WIDTH(10),
               ,.OUT_t(upper_t))
	    instName
	     (/*AUTOINST*/
	      // Outputs
	      .o			(o),
	      // Inputs
	      .i			(i[9:0]));
        endmodule

// Local Variables:
// verilog-typedef-regexp: "_[tT]$"
// verilog-auto-inst-param-value:t
// verilog-auto-inst-param-value-type:t
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_[tT]\\>"
// End:
// New File: <autoinst_param_type.v>
module InstMod ( ins, outs );
   parameter WIDTH;
   output [WIDTH-1:0] ins;
endmodule

module test_top;
   parameter TOP_WIDTH = 3;
   /* AUTO_LISP(defun my-param-range ()
                 (concat "[" vh-TOP_WIDTH ":0]"))*/

   /* InstMod AUTO_TEMPLATE(
        .WIDTH(@"vh-TOP_WIDTH"),
        .ins(ins@"(my-param-range)"),
      ) */

   InstMod mod
    #(/*AUTOINSTPARAM*/
      // Parameters
      .WIDTH                            (3))                     // Templated
    (/*AUTOINST*/
     // Outputs
     .ins                               (ins[3:0]));              // Templated

endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autoinst_param_value.v>
module  testmuxpv();
   # (parameter WIDTH = 32)
   (
    input wire [2:0]    /* synopsys enum cur_info */ sel,
    input wire [WIDTH-1:0] a,
    output reg [WIDTH-1:0] out
    );
endmodule

module  top_test();

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [WIDTH-1:0]	out;			// From testmuxpv_boo of testmuxpv.v, ..., Couldn't Merge
   // End of automatics

   //======================================================================

   /* testmuxpv AUTO_TEMPLATE (
    ) */

   testmuxpv #(.IGNORE((1)),
	       .WIDTH(  16  ),
	       .IGNORE2(2))
   testmuxpv_boo
     (/*AUTOINST*/
      // Outputs
      .out				(out[15:0]),
      // Inputs
      .sel				(sel[2:0]),
      .a				(a[15:0]));

   //======================================================================

   testmuxpv #(.IGNORE((1)),
	       .WIDTH(WIDTH),   // Make sure we don't recurse!
	       .IGNORE2(2))
   testmuxpv_boo
     (/*AUTOINST*/
      // Outputs
      .out				(out[WIDTH-1:0]),
      // Inputs
      .sel				(sel[2:0]),
      .a				(a[WIDTH-1:0]));

   //======================================================================
   // bug331: vl-width should correct when param values propagating

   /* testmuxpv AUTO_TEMPLATE (
    .a ({@"vl-width"{1'b0}}),
    ) */

   testmuxpv #(.IGNORE((1)),
	       .WIDTH(16),
	       .IGNORE2(2))
   testmuxpv_boo
     (/*AUTOINST*/
      // Outputs
      .out				(out[15:0]),
      // Inputs
      .sel				(sel[2:0]),
      .a				({16{1'b0}}));		 // Templated

endmodule

// Local Variables:
// verilog-auto-inst-param-value:t
// End:
// New File: <autoinst_paramvalue.v>
module io1_sub(
	       /*AUTOARG*/);

   /* autoinst_lopaz_srpad AUTO_TEMPLATE (
    .pin_in(),
    // Inputs
    .pin_out (),
    ); */

   autoinst_lopaz_srpad i_ctrl
     (/*AUTOINST*/
      // Outputs
      .pin_in				(),			 // Templated
      // Inouts
      .pin				(pin[w-1:0]),
      // Inputs
      .clk				(clk),
      .pin_out				(),			 // Templated
      .pin_outen			(pin_outen));

endmodule
// New File: <autoinst_podolsky.v>
module autoinst_precomment (
			    why,
			    /*AUTOARG*/
   // Inputs
   nnnot
   );
   input why;
   input nnnot;

   autoinst_wildcard_sub sub0
     (
      .sd_ras_				(foobar_ras_),
      //.sd0_dqm7_l			(dontdome),
      /*AUTOINST*/
      // Inouts
      .sd0_dqm7_l			(sd0_dqm7_l),
      .sd0_dqm6_l			(sd0_dqm6_l),
      .sd0_dqm5_l			(sd0_dqm5_l),
      .sd0_dqm4_l			(sd0_dqm4_l),
      .sd0_dqm3_l			(sd0_dqm3_l),
      .sd0_dqm2_l			(sd0_dqm2_l),
      .sd0_dqm1_l			(sd0_dqm1_l),
      .sd0_dqm0_l			(sd0_dqm0_l),
      .sd0_ba1				(sd0_ba1),
      .sd0_ba0				(sd0_ba0),
      .sd0_adrs11			(sd0_adrs11),
      .sd0_adrs10			(sd0_adrs10),
      .sd0_adrs9			(sd0_adrs9),
      .sd0_adrs8			(sd0_adrs8),
      .sd0_adrs7			(sd0_adrs7),
      .sd0_adrs6			(sd0_adrs6),
      .sd0_adrs5			(sd0_adrs5),
      .sd0_adrs4			(sd0_adrs4),
      .sd0_adrs3			(sd0_adrs3),
      .sd0_adrs2			(sd0_adrs2),
      .sd0_adrs1			(sd0_adrs1),
      .sd0_adrs0			(sd0_adrs0),
      .sd0_clk				(sd0_clk));

endmodule
// New File: <autoinst_precomment.v>
module foo();
   bar i0 (
           /*AUTOINST*/
           // Inputs
           .a                           (a));
endmodule // foo

module bar(input logic a);
`protected
  input notreally;
`endprotected

`pragma protect begin_protected
`pragma protect data_block
   AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA[
`pragma protect end_protected

 endmodule // bar
// New File: <autoinst_protected.v>
module temp2(/*AUTOARG*/);

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [5:0]		out;			// From adc_3 of adc.v
   // End of automatics

   adc adc_3 (
	      // Outputs
	      .out			(out[5:0]),
	      // Inputs
	      .Vi			(Vi),
	      // Inputs
	      .evalClk			(evalClk),
              /*AUTOINST*/);

endmodule // temp


module adc(/*AUTOARG*/
   // Outputs
   out,
   // Inputs
   Vi, evalClk
   );
   output [5:0] out;
   input 	Vi;
   input 	evalClk;
endmodule
// New File: <autoinst_rao.v>
module sub;
   parameter AA, AB, AC;
   output ia, ib, ic;
endmodule

module autoinst_regexp_inst;

   /*AUTOWIRE*/

   // We don't yet support AUTO INST with a parameter

   sub
     #(/*AUTOINSTPARAM("AB")*/)
   sub0
     (/*AUTOINST*/);

   sub
   sub1
     (/*AUTOINST("ia")*/);

   sub
   sub1
     (/*AUTOINST("?!ia")*/);

endmodule
// New File: <autoinst_regexp_inst.v>
module autoinst_wildcard;

   /*AUTOINOUT*/

   /* autoinst_wildcard_sub AUTO_TEMPLATE (
    .sd\([0-9]\)_\(.*\)\([0-9]+\)\(.*\)	(@"(uc \\"sd_\2\4[\1][\3]\\")"),
    ); */

   /*AUTOOUTPUT*/

   autoinst_wildcard_sub sub0
     (/*AUTOINST*/);

   /* autoinst_wildcard_sub AUTO_TEMPLATE (
    .sd\([0-9]\)_\(.*\)\([0-9]+\)\(.*\)	(sd_\2\4[\1][\3]),
    ); */

   /*AUTOOUTPUT*/

   autoinst_wildcard_sub sub1
     (/*AUTOINST*/);

endmodule

// Local Variables:
// eval:(defun uc (x) (upcase x))
// End:
// New File: <autoinst_regexp_match.v>
module  testmux();
   # (parameter WIDTH = 32)
   (
    input wire [2:0]    /* synopsys enum cur_info */ sel,
    input wire [WIDTH-1:0] a,
    output reg [WIDTH-1:0] out
    );
endmodule

module  top_test();

   /*AUTOWIRE*/

   /*AUTO_LISP(setq verilog-auto-inst-param-value nil)*/

   /* testmux AUTO_TEMPLATE "testmux_\(.*\)" (
    .a (@_a_symbolic[]),
    .out (@_out_symbolic[]),
    );
    */

   testmux #(.WIDTH(  16  )) testmux_boo
     (/*AUTOINST*/);

   testmux  testmux_defaultwidth
     (/*AUTOINST*/);

   //======================================================================

   /*AUTO_LISP(setq verilog-auto-inst-param-value t)*/

   /* testmux AUTO_TEMPLATE "testmux_\(.*\)" (
    .a (@_a_value[]),
    .out (@_out_value[]),
    );
    */

   testmux #(.IGNORE((1)),
	     .WIDTH(  16  ),
	     .IGNORE2(2))
     testmux_boo
       (/*AUTOINST*/);

   //======================================================================

   testmux #(.IGNORE((1)),
	     .WIDTH(WIDTH),   // Make sure we don't recurse!
	     .IGNORE2(2))
     testmux_boo
       (/*AUTOINST*/);

endmodule
// New File: <autoinst_rogoff.v>
module autoinst_rons;
   dwrr dwrr_inst (/*AUTOINST*/);
endmodule

// module declaration
module dwrr (
             //Inputs
    input [47:0] data_avail,
    input [47:0] cell_eof,

             //Outputs
    output reg [6:0]  dwell_count_out,
    output reg [47:0] eligible_flag,
    output [5:0]      dwrr_state );

endmodule
// New File: <autoinst_rons.v>

module autoinst_signed_fubar2
  (
   input  [1:0] an_input2,
   output  [1:0] an_output2
   output  [1:0] another_output2
   );

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg [1:0]		an_output2;
   reg [1:0]		another_output2;
   // End of automatics

endmodule
// New File: <autoinst_signed_fubar2.v>

module autoinst_signed_fubar
  (
   input signed [1:0] an_input,
   output signed [1:0] an_output
   output signed [1:0] another_output
   );

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg signed [1:0]	an_output;
   reg signed [1:0]	another_output;
   // End of automatics

endmodule
// New File: <autoinst_signed_fubar.v>
module autoinst_signed
  (/*AUTOARG*/
   // Outputs
   another_output2, another_output, an_outputpre, an_output2
   );

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output [1:0]		an_output2;		// From u_fubar2 of autoinst_signed_fubar2.v
   output signed [1:0]	an_outputpre;		// From u_fubar of autoinst_signed_fubar.v
   output signed [1:0]	another_output;		// From u_fubar of autoinst_signed_fubar.v
   output [1:0]		another_output2;	// From u_fubar2 of autoinst_signed_fubar2.v
   // End of automatics

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [1:0]		an_output2;		// From u_fubar2 of autoinst_signed_fubar2.v
   wire signed [1:0]	an_outputpre;		// From u_fubar of autoinst_signed_fubar.v
   wire signed [1:0]	another_output;		// From u_fubar of autoinst_signed_fubar.v
   wire [1:0]		another_output2;	// From u_fubar2 of autoinst_signed_fubar2.v
   // End of automatics

   autoinst_signed_fubar u_fubar
     (
      // Outputs
      .an_output			(an_outputpre[1:0]),
      .plover (plump),
      /*AUTOINST*/
      // Outputs
      .another_output			(another_output[1:0]),
      // Inputs
      .an_input				(an_input[1:0]));

   autoinst_signed_fubar2 u_fubar2
     (
      /*AUTOINST*/
      // Outputs
      .an_output2			(an_output2[1:0]),
      .another_output2			(another_output2[1:0]),
      // Inputs
      .an_input2			(an_input2[1:0]));

endmodule
// New File: <autoinst_signed.v>
module unsort(
   input  t,
   input  a,
   input  z,

   output o,
   output b);
endmodule

module t;
   unsort sub (/*AUTOINST*/
               // Outputs
               .b                       (b),
               .o                       (o),
               // Inputs
               .a                       (a),
               .t                       (t),
               .z                       (z));
endmodule

// Local Variables:
// verilog-auto-inst-sort: t
// End:
// New File: <autoinst_sort.v>
module io1_sub (/*AUTOARG*/);

   /*AUTOWIRE*/

   autoinst_lopaz_srpad MD31_pad
     (.*,
      .foo (touch_this_not_my_pretty));

   /* autoinst_lopaz_srpad AUTO_TEMPLATE (
    ); */

   autoinst_lopaz_srpad MD31_pad
     (.*);

  /* autoinst_lopaz_srpad AUTO_TEMPLATE (
      .pin	(templated));
    */

   autoinst_lopaz_srpad MD31_pad
     (.*);

   // And .name with auto inst
   autoinst_lopaz_srpad MD31_pad22
     (.pin,
      .clk,
      /*AUTOINST*/);

   always @(posedge clk) begin
      $display ("This .* shouldn't expand.\n");
   end

endmodule
// New File: <autoinst_star.v>
`timescale 1ns/100ps

// -----------------------------------------------------------------------------
// Leaf module using multi-dimensional array ports
// -----------------------------------------------------------------------------
module autoinst_sv_kulkarni_base
  // Verilog 2001 style
  #(parameter M=5, N=3)
    (
    output logic [N-1:0][M-1:0] a_o1,
    input [N-1:0][M-1:0] a_i1
     );

   // -----------------------------------------------------------------------------
   // Main Code
   always_comb begin
      for (int i=0; i<N; i++)
        a_o1[i] = ^(a_i1[i]);
   end

endmodule
// New File: <autoinst_sv_kulkarni_base.v>
`timescale 1ns/100ps

// -----------------------------------------------------------------------------
// One-level up Hierarchical module
// -----------------------------------------------------------------------------
module a_h
  // Verilog 2001 style
  #(parameter M=5, N=3)
    (
     // Outputs
     output [N-1:0] [M-1:0]a_o1		// From Ia of autoinst_sv_kulkarni_base.v
     // End of automatics
     // AUTOINPUT*/
     );

   /*AUTOWIRE*/

   autoinst_sv_kulkarni_base
     #(/*AUTOINSTPARAM*/)
     Ia
       (/*AUTOINST*/); // <---- BUG?

endmodule

// -----------------------------------------------------------------------------
// Top-level module or Testbench
// -----------------------------------------------------------------------------
module top;
   parameter M=4;
   parameter N=2;

   wire [N-1:0]         a_o1;
   logic [N-1:0][M-1:0] a_i1;

   logic                temp;

   /*AUTOWIRE*/

   // Workaround to fix multi-dimensional port problem
   // a) Set "verilog-auto-inst-vector = nil"
   // b)  ----> a_h AUTO_TEMPLATE ( .\(.*\)   (\1), ); */

   a_h #(/*AUTOINSTPARAM*/)
     Ua_h
     (/*AUTOINST*/);    // <---- BUG?

   // Stimulus
   initial begin
      a_i1 = { 4'h0, 4'h2 };
      #5;
      $display("Loop Init: a_i1 = { %h, %h }   a_o1 = %h\n",
               a_i1[1], a_i1[0], a_o1);
      #5;
      for (int i=0; i<1; i++) begin
         for (int j=0; j<N; j++) begin
            temp = 1'b0;
            for (int k=0; k<M; k++) begin
               a_i1[j][k] = temp;
               temp = ~temp;
            end
         end
         #5;
         $display("Loop %0d: a_i1 = { %h, %h }   a_o1 = %h\n",
                  i, a_i1[1], a_i1[0], a_o1);
         #5;
      end
   end

endmodule
// New File: <autoinst_sv_kulkarni.v>
`timescale 1ns/100ps

module a_h
  #(parameter M=5, N=3)
    ();

   /*AUTOWIRE*/

   autoinst_sv_kulkarni_base
     #(/*AUTOINSTPARAM*/)
     Ia
       (/*AUTOINST*/); // <---- BUG?

endmodule
// New File: <autoinst_sv_kulkarni_wire.v>
module autoinst_sv_shaw
  (
   /*AUTOINOUTMODULE("Example_mod")*/
   );

   Example_mod Ex1 (/*AUTOINST*/);

endmodule

module Example_mod
  (
    input  logic clk,
    input  logic reset_b,
   );
endmodule

// New File: <autoinst_sv_shaw.v>
// bug1242
// A and B will swap. We don't have a choice or will break autoinst_clog2_bug522.v

module test_submodule #
   (
    parameter A_WL = 16,
    parameter B_WL = 16
    )
   (
    input logic                    aclk,
    input logic signed [A_WL-1:0]  a_tdata,
    input logic                    a_tvalid,
    input logic signed [B_WL-1:0]  b_tdata,
    input logic                    b_tvalid
    );

endmodule : test_submodule

module test_top #
  (
    parameter int A_WL = 16,
    parameter int B_WL = 32
    )
   (

    input logic                    aclk,

    input logic signed [A_WL-1:0]  a_tdata,
    input logic                    a_tvalid,

    input logic signed [B_WL-1:0]  b_tdata,
    input logic                    b_tvalid
    );

   /* test_submodule AUTO_TEMPLATE (
    .A_\(.*\)                           (B_\1),
    .B_\(.*\)                           (A_\1),
    .a_\(.*\)                           (b_\1[]),
    .b_\(.*\)                           (a_\1[]),
    ); */

   test_submodule
      #(/*AUTOINSTPARAM*/)
   test_submodule_inst
      (/*AUTOINST*/);

endmodule : test_top

// Local Variables:
// verilog-auto-inst-param-value:t
// End:
// New File: <autoinst_swapped_vec.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2011 by Wilson Snyder.

// Reported by Julian Gorfajn <jig1@cornell.edu>

module autoinst_multitemplate ();

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		Boo2;			// To suba2 of SubB.v
   input		Boo3;			// To suba3 of SubB.v
   input		a;			// To suba2 of SubB.v
   input		b;			// To suba3 of SubB.v
   // End of automatics

   /*AUTOOUTPUT*/

   /*AUTOWIRE*/

   wire [3:0] 		f4_dotnamed;

   /*
     SubB AUTO_TEMPLATE (
     .b (Boo@),
    );*/

   SubB suba2 (/*AUTOINST*/
	       // Inputs
	       .a			(a),
	       .b			(Boo2));			 // Templated

   /*
     SubB AUTO_TEMPLATE (
     .a (Boo@),
    );*/

   SubB suba3 (/*AUTOINST*/
	       // Inputs
	       .a			(Boo3),			 // Templated
	       .b			(b));

// Test harness doesn't support expected errors
//   /*
//     SubUnused AUTO_TEMPLATE (
//     .subunused (Boo@),
//    );*/

endmodule

module SubB (input a,input b);
endmodule

// Local Variables:
// verilog-auto-template-warn-unused: t
// End:
// New File: <autoinst_template_lint.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2020 by Wilson Snyder.

module autoinst_multitemplate ();

   /*
     SubB AUTO_TEMPLATE (
     .b (Boo@),
     .other (Other),
    );*/

   SubB subb (/*AUTOINST*/
              // Inputs
              .b                        (Boo),                   // Templated
              .other                    (Other));                // Templated

   SubC subc (/*AUTOINST*/);

endmodule

module SubB (input not_listed, input b, input other);
endmodule

module SubC (input also_not_listed, input b);
endmodule

// Local Variables:
// verilog-auto-inst-template-required: t
// End:
// New File: <autoinst_template_required.v>
typedef logic [3:0][1:0] sometype_t;

module top
  #(parameter X=4,
    parameter Y=1)

   (input clk,
    input rstb,

    /*AUTOINPUT("^x.*\|v.*")*/
    /*AUTOOUTPUT("^c.*\|k.*")*/

    /*AUTOOUTPUT("^y.*")*/

    /*AUTOINPUT*/
    /*AUTOOUTPUT*/
    // Beginning of automatic outputs (from unused autoinst outputs)
    output		foobar,			// From XX of xx.v
    output [4:0] [2:0]	foobar2		// From YY of yy.v
    // End of automatics
    );

   xx (/*AUTOINSTPARAM*/
       // Parameters
       .X				(X),
       .Y				(Y))
     XX(/*AUTOINST*/
	// Outputs
	.st				(st[1:0]),
	.foobar				(foobar),
	// Inputs
	.clk				(clk),
	.rstb				(rstb),
	.xc				(xc/*[X-1:0][1:0]*/),
	.xa				(xa[X-1:0]),
	.xb				(xb[X-1:0]),
	.cb				(cb[X-1:0]),
	.yb				(yb[X*Y-1:0]));
   
   yy (/*AUTOINSTPARAM*/
       // Parameters
       .X				(X),
       .Y				(Y))
     YY(/*AUTOINST*/
	// Outputs
	.xc				(xc/*[X-1:0][1:0]*/),
	.xa				(xa[X-1:0]),
	.yb				(yb[X*Y-1:0]),
	.foobar2			(foobar2/*[4:0][2:0]*/),
	// Inputs
	.clk				(clk),
	.rstb				(rstb),
	.xb				(xb[X-1:0]),
	.cb				(cb[X-1:0]),
	.st				(st[1:0]));

endmodule // top

module xx
  #(parameter X=4,
    parameter Y=1)
   (input clk,
    input rstb,

    input [X-1:0][1:0] xc,
    input [X-1:0] xa,
    input [X-1:0] xb,

    input [X-1:0] cb,
    output sometype_t [1:0]  st,

    input [X*Y-1:0] yb,

    output foobar
    );

endmodule // xx

module yy
  #(parameter X=4,
    parameter Y=1)
   (input clk,
    input rstb,

    output [X-1:0][1:0] xc,
    output [X-1:0] xa,
    input [X-1:0] xb,

    input [X-1:0] cb,
    input         sometype_t [1:0] st,

    output [X*Y-1:0] yb,

    output [4:0][2:0] foobar2
    );

endmodule // xx

// Local Variables:
// verilog-typedef-regexp:"_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// verilog-library-directories:("." )
// verilog-library-extensions:(".v" ".sv" ".h" ".vr" ".vm")
// End:
// New File: <autoinst_tennant.v>
module autotieoff_signed (/*AUTOARG*/);

   /*AUTO_LISP(setq my-nc-output "\/*NC*\/"  my-nc-input-scalar "1'b0"   my-nc-input-vector "'0"  my-nc-input-mdv "'{default:'0}"  my-space "|")*/

     /* sub AUTO_TEMPLATE (
       .\(.*\)    (@"(concat (if (equal vl-dir \\"output\\") my-nc-output  (if (not vl-memory) my-nc-input-vector  my-nc-input-mdv) )  my-space vl-width my-space vl-bits my-space vl-mbits my-space vl-memory )"),
	) */

   // Value | vl-width | vl-bits | vl-mbits | vl-memory
   sub sub (/*AUTOINST*/
	    // Outputs
	    .outvar			(/*NC*/|1|||),		 // Templated
	    // Inputs
	    .scalar_var			('0|1|||),		 // Templated
	    .packed1_var		('0|2|[1:0]||),		 // Templated
	    .packed2_var		('0|3|[2:0]|[1:0]|),	 // Templated
	    .packed1_unpacked1_var	('{default:'0}|2|[1:0]||[2]), // Templated
	    .packed1_unpacked2_var	('{default:'0}|2|[1:0]||[2][3]), // Templated
	    .packed2_unpacked1_var	('{default:'0}|3|[2:0]|[1:0]|[2]), // Templated
	    .packed2_unpacked2_var	('{default:'0}|3|[2:0]|[1:0]|[2][3]), // Templated
	    .unpacked1_var		('{default:'0}|1|||[2]), // Templated
	    .unpacked2_var		('{default:'0}|1|||[2][3])); // Templated

endmodule

module sub
  (
   output outvar,
   input wire 		 scalar_var,
   input wire [1:0] 	 packed1_var,
   input wire [1:0][2:0] packed2_var,
   input wire [1:0] 	 packed1_unpacked1_var [2],
   input wire [1:0] 	 packed1_unpacked2_var [2][3],
   input wire [1:0][2:0] packed2_unpacked1_var [2],
   input wire [1:0][2:0] packed2_unpacked2_var [2][3],
   input wire 		 unpacked1_var [2]
   input wire 		 unpacked2_var [2][3]
   );
endmodule

// Local Variables:
// verilog-active-low-regexp: "_l$"
// verilog-auto-tieoff-ignore-regexp: "ignored"
// End:
// New File: <autoinst_tieoff_vec.v>
module autoinst_unsigned_bug302;

   Sub #(/*AUTOINSTPARAM*/) u_Abc
     (/*AUTOINST*/);

endmodule

module Sub
  #(
    parameter No1 = 6,
    parameter int unsigned No2                                                  // Parameter no. 2
                       = pa_Abc::No2,
    parameter bit          No3 [No1:0][No2-1:0]                                 // Parameter no. 3
                       = pa_Abc::No3
  )

   (
    input  logic ck,
    input  logic [No1-1:0][31:0] abc
    input  logic [No1-1:0][31:0] abc
    );

endmodule
// New File: <autoinst_unsigned_bug302.v>
module sub
  (
   output signed [3:0] as,
   output [3:0]        bs);
endmodule

module t;
  wire signed [3:0]	as;			// From sub of sub.v
  wire [3:0]		bs;			// From sub of sub.v

  sub sub (/*AUTOINST*/
	   // Outputs
	   .as				(as),
	   .bs				(bs));
endmodule

// Local Variables:
// verilog-auto-inst-vector: nil
// End:
// New File: <autoinst_vec_nil.v>
module sub
  (
   output signed [3:0] as,
   output [3:0] 	       bs);
endmodule

module t;
  wire signed [3:0]	as;			// From sub of sub.v
  wire [3:0]		bs;			// From sub of sub.v

  sub sub (/*AUTOINST*/
	   // Outputs
	   .as				(as[3:0]),
	   .bs				(bs[3:0]));
endmodule

// Local Variables:
// verilog-auto-inst-vector: t
// End:
// New File: <autoinst_vec_t.v>
module sub
  (
   output signed [3:0] as,
   output [3:0] 	       bs);
endmodule

module t;
  wire signed [3:0]	as;			// From sub of sub.v
  wire [3:0]		bs;			// From sub of sub.v

  sub sub (/*AUTOINST*/
	   // Outputs
	   .as				(as),
	   .bs				(bs[3:0]));
endmodule

// Local Variables:
// verilog-auto-inst-vector: unsigned
// End:
// New File: <autoinst_vec_unsigned.v>

module autoinst_vertrees_slv
  (/*AUTOARG*/
   // Outputs
   i2c_mst_data, i2c_read, i2c_slv_scl_out, i2c_slv_sda_out, i2c_spc_scl_state, i2c_start,
   i2c_strobe, slv_bit_st, slv_put, i2c_spc_scl_fall, i2c_spc_sda_state, i2c_spc_start,
   i2c_spc_stop,
   // Inputs
   ck_ref, i2c_slv_data, i2c_slv_scl_in, i2c_slv_sda_in, r_i2c_spc_scl_low, rpt_hold, rpt_sda,
   rst_ref, test_mode
   );

   input                 ck_ref;                 // To u_spc of i2c_slv_pin_ctrl.v, ...
   input [7:0] 		 i2c_slv_data;           // To u_misc of ddc_slv_misc.v
   input                 i2c_slv_scl_in;         // To u_spc of i2c_slv_pin_ctrl.v
   input                 i2c_slv_sda_in;         // To u_spc of i2c_slv_pin_ctrl.v
   input [4:0] 		 r_i2c_spc_scl_low;      // To u_spc of i2c_slv_pin_ctrl.v
   input                 rpt_hold;               // To u_misc of ddc_slv_misc.v
   input                 rpt_sda;                // To u_misc of ddc_slv_misc.v
   input                 rst_ref;                // To u_spc of i2c_slv_pin_ctrl.v, ...
   input                 test_mode;              // To u_spc of i2c_slv_pin_ctrl.v
   
   output [7:0] 	 i2c_mst_data;           // From u_misc of ddc_slv_misc.v
   output                i2c_read;               // From u_misc of ddc_slv_misc.v
   output                i2c_slv_scl_out;        // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_slv_sda_out;        // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_spc_scl_state;      // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_start;              // From u_misc of ddc_slv_misc.v
   output                i2c_strobe;             // From u_misc of ddc_slv_misc.v
   output                slv_bit_st;             // From u_misc of ddc_slv_misc.v
   output                slv_put;                // From u_misc of ddc_slv_misc.v
   output                i2c_spc_scl_fall;       // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_spc_sda_state;      // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_spc_start;          // From u_spc of i2c_slv_pin_ctrl.v
   output                i2c_spc_stop;           // From u_spc of i2c_slv_pin_ctrl.v
   
endmodule // ddc_slv
// New File: <autoinst_vertrees_slv.v>
module autoinst_vertrees
  (/*AUTOARG*/);

  /*AUTOINPUT*/

  /*AUTOOUTPUT*/

  /*AUTOWIRE*/

  /*
   autoinst_vertrees_slv AUTO_TEMPLATE "u_slv_\([a-z]\)"
    (.i2c_\(.*\) (i2c_@_\1),
     .slv_\(.*\) (slv_@_\1),
     .rpt_\(.*\) (rpt_@_\1),
     )
  */  
  autoinst_vertrees_slv u_slv_a
    (/*AUTOINST*/);   

  autoinst_vertrees_slv u_slv_b
    (/*AUTOINST*/); 

endmodule // ddc
// New File: <autoinst_vertrees.v>
module autoinst_vkadamby ( /*AUTOARG*/);

   child1 #(.lengthM2(100))  I34 (/*AUTOINST*/
				  // Inputs
				  .x			(x));

endmodule

module child1 (/*AUTOARG*/
   // Inputs
   x
   );

   input x;
   parameter lengthM2;

endmodule

// New File: <autoinst_vkadamby.v>
module ex;

   /* subwidth AUTO_TEMPLATE (
    .bus1a(@"vl-width"'b0),
    .bus1b(@"vl-width"'b0),
    .bus4a(@"vl-width"'b0),
    .bus4b(@"vl-width"'b0),
    .busdef(@"vl-width"'b0),
    );*/

   subwidth u_a2(/*AUTOINST*/
		 // Outputs
		 .bus4a			(4'b0),			 // Templated
		 .bus4b			(4'b0),			 // Templated
		 .bus1a			(1'b0),			 // Templated
		 .bus1b			(1'b0),			 // Templated
		 .busdef		((1+(`defa)-(`defb))'b0)); // Templated

endmodule

module subwidth (/*AUTOARG*/
   // Outputs
   bus4a, bus4b, bus1a, bus1b, busdef
   );

   output [0:3] bus4a;
   output [7:4] bus4b;
   output       bus1a;
   output [0:0] bus1b;
   output [`defa:`defb] busdef;

endmodule
// New File: <autoinst_width.v>
module autoinst_wildcard_repeat;

   /* sub AUTO_TEMPLATE (
    .w_\(.*\)_\1   (match_\1),
    .w_\(.*\)   (hit_\1),
    ); */

   sub sub
     (/*AUTOINST*/
      // Inputs
      .w_a_a				(match_a),		 // Templated
      .w_bb_bb				(match_bb),		 // Templated
      .w_x_y				(hit_x_y));		 // Templated

endmodule

module sub;
   input w_a_a;
   input w_bb_bb;
   input w_x_y;
endmodule
// New File: <autoinst_wildcard_repeat.v>
module autoinst_wildcard_sub (/*AUTOARG*/
   // Inouts
   sd_ras_, sd0_dqm7_l, sd0_dqm6_l, sd0_dqm5_l, sd0_dqm4_l, sd0_dqm3_l,
   sd0_dqm2_l, sd0_dqm1_l, sd0_dqm0_l, sd0_ba1, sd0_ba0, sd0_adrs11,
   sd0_adrs10, sd0_adrs9, sd0_adrs8, sd0_adrs7, sd0_adrs6, sd0_adrs5,
   sd0_adrs4, sd0_adrs3, sd0_adrs2, sd0_adrs1, sd0_adrs0, sd0_clk
   );

   //======================================================================
   // Inputs/Outputs
   //======================================================================

   inout  sd_ras_;         // SDRAM Channel 0 Row Address Strobe
   inout  sd0_dqm7_l;         // SDRAM Channel 0 DQM Mask Bits
   inout  sd0_dqm6_l;
   inout  sd0_dqm5_l;
   inout  sd0_dqm4_l;
   inout  sd0_dqm3_l;
   inout  sd0_dqm2_l;
   inout  sd0_dqm1_l;
   inout  sd0_dqm0_l;
   inout  sd0_ba1;
   inout  sd0_ba0;
   inout  sd0_adrs11;       // SDRAM Channel 0 Address
   inout  sd0_adrs10;
   inout  sd0_adrs9;
   inout  sd0_adrs8;
   inout  sd0_adrs7;
   inout  sd0_adrs6;
   inout  sd0_adrs5;
   inout  sd0_adrs4;
   inout  sd0_adrs3;
   inout  sd0_adrs2;
   inout  sd0_adrs1;
   inout  sd0_adrs0;
   inout  sd0_clk;         // SDRAM Channel 0 Clocks

endmodule

// Local Variables:
// fill-column: 79
// End:
// New File: <autoinst_wildcard_sub.v>
module autoinst_wildcard;

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics

   // Newline between AUTO_TEMPLATE and ( gave a templated line number bug
   /* autoinst_wildcard_sub AUTO_TEMPLATE
    (
    .sd0_clk	(sd_clk),
    .sd0_dqm\(.*\)_l	(c@_sd_dqm_[\1]),
    .sd0_ba\([0-9]+\)	(c@_sd_ba[\1]),
    .sd0_adrs@	(c@_sd_adrs[\1]),
    .ics@		(c@_ics[\1]),
    ); */

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics

   autoinst_wildcard_sub sub0
     (/*AUTOINST*/
      // Inouts
      .sd_ras_				(sd_ras_),
      .sd0_dqm7_l			(c0_sd_dqm_[7]),	 // Templated 9
      .sd0_dqm6_l			(c0_sd_dqm_[6]),	 // Templated 9
      .sd0_dqm5_l			(c0_sd_dqm_[5]),	 // Templated 9
      .sd0_dqm4_l			(c0_sd_dqm_[4]),	 // Templated 9
      .sd0_dqm3_l			(c0_sd_dqm_[3]),	 // Templated 9
      .sd0_dqm2_l			(c0_sd_dqm_[2]),	 // Templated 9
      .sd0_dqm1_l			(c0_sd_dqm_[1]),	 // Templated 9
      .sd0_dqm0_l			(c0_sd_dqm_[0]),	 // Templated 9
      .sd0_ba1				(c0_sd_ba[1]),		 // Templated 10
      .sd0_ba0				(c0_sd_ba[0]),		 // Templated 10
      .sd0_adrs11			(c0_sd_adrs[11]),	 // Templated 11
      .sd0_adrs10			(c0_sd_adrs[10]),	 // Templated 11
      .sd0_adrs9			(c0_sd_adrs[9]),	 // Templated 11
      .sd0_adrs8			(c0_sd_adrs[8]),	 // Templated 11
      .sd0_adrs7			(c0_sd_adrs[7]),	 // Templated 11
      .sd0_adrs6			(c0_sd_adrs[6]),	 // Templated 11
      .sd0_adrs5			(c0_sd_adrs[5]),	 // Templated 11
      .sd0_adrs4			(c0_sd_adrs[4]),	 // Templated 11
      .sd0_adrs3			(c0_sd_adrs[3]),	 // Templated 11
      .sd0_adrs2			(c0_sd_adrs[2]),	 // Templated 11
      .sd0_adrs1			(c0_sd_adrs[1]),	 // Templated 11
      .sd0_adrs0			(c0_sd_adrs[0]),	 // Templated 11
      .sd0_clk				(sd_clk));		 // Templated 8

endmodule

// Local Variables:
// verilog-auto-inst-template-numbers: t
// End:
// New File: <autoinst_wildcard.v>
module autoinst_wildcard;

   /* sub AUTO_TEMPLATE (
    .a\(.\)\(.\)  (@"(substring vl-cell-name 4 5)"_@"(substring vl-cell-name 6 7)"_a_\1_\2),
    ); */

   sub sub_0_0 (/*AUTOINST*/);
   sub sub_0_1 (/*AUTOINST*/);
   sub sub_1_0 (/*AUTOINST*/);
   sub sub_1_1 (/*AUTOINST*/);

   /* sub AUTO_TEMPLATE (
    .a\(.\)\(.\)  (b_\1_\2),
    ); */

   sub sub_0_0 (/*AUTOINST*/);
   sub sub_0_1 (/*AUTOINST*/);
   sub sub_1_0 (/*AUTOINST*/);
   sub sub_1_1 (/*AUTOINST*/);

endmodule

module sub;
   input a33, a34, a44, a43;
endmodule

// Local Variables:
// verilog-auto-inst-template-numbers: t
// End:
// New File: <autoinst_wildcell.v>
// See forum topic 176
module autolisp_include (/*AUTOARG*/
   // Outputs
   foo,
   // Inputs
   bar
   );

   //`include "autolisp_include_inc.vh"
   /*AUTOINSERTLISP(verilog-library-filenames (insert-file "autolisp_include_inc.vh"))*/
   // Beginning of automatic insert lisp
  input bar;
  output foo;
   // End of automatics

   assign foo = bar;

   // This doesn't need commentary
   /*AUTOINSERTLISP(when nil)*/

endmodule
// New File: <autolisp_include.v>
module autolisp_top (/*AUTOARG*/);

/* autolisp_inst AUTO_TEMPLATE (
   .\(.*\)A    (\1_@"(eval tense)"_A),
   .\(.*\)B    (\1_@"(eval tense)"_B),
  );
*/
   /* AUTO_LISP(setq tense "is") */
   autolisp_inst AUTOLISP_INST_I0
     (/*AUTOINST*/
      // Outputs
      .result				(result),
      // Inputs
      .portA				(port_is_A),		 // Templated
      .busA				(bus_is_A),		 // Templated
      .portB				(port_is_B),		 // Templated
      .busB				(bus_is_B));		 // Templated

   /* AUTO_LISP(setq tense "was") */
   autolisp_inst AUTOLISP_INST_I1
     (/*AUTOINST*/
      // Outputs
      .result				(result),
      // Inputs
      .portA				(port_was_A),		 // Templated
      .busA				(bus_was_A),		 // Templated
      .portB				(port_was_B),		 // Templated
      .busB				(bus_was_B));		 // Templated

endmodule


module autolisp_inst (/*AUTOARG*/
   // Outputs
   result,
   // Inputs
   portA, busA, portB, busB
   );

   input       portA;
   input [3:0] busA;
   input       portB;
   input [1:0] busB;

   output      result;

endmodule
// New File: <autolisp_order_bug356.v>
module ExampInsertLisp;

   /*AUTOINSERTLISP(my-verilog-insert-hello "world")*/

   // Try an empty insert
   /*AUTOINSERTLISP(+ 1)*/

   // Try a shell command
   /*AUTOINSERTLISP(insert (shell-command-to-string "echo //hello"))*/

endmodule
/*
 Local Variables:
 eval:
   (defun my-verilog-insert-hello (who)
     (insert (concat "initial $write(\"hello " who "\");\n")))
 End:
*/
// New File: <autolisp_truex.v>
module sub1 
  #(
    parameter integer AAA= {32'd4,32'd8}, 
    parameter integer BBB= 1
    )
   (
    output [AAA[BBB]-1:0] out_signal
    );
endmodule

module top 
  #(
    parameter integer AAA = {32'd4,32'd8}, 
    parameter integer BBB = 1 
    )
   (
    );

   /*AUTOLOGIC*/

   logic [AAA[BBB]    -1:0] out_signal;             // From sub1 of sub1.v

   sub1 #(/*AUTOINSTPARAM*/
	  // Parameters
	  .AAA				(AAA),
	  .BBB				(BBB))
   sub1 (/*AUTOINST*/
	 // Outputs
	 .out_signal			(out_signal[AAA[BBB]-1:0]));


endmodule
// New File: <autologic_bracket.v>
module bug(output [7:0] a);

   initial begin
      a = 0;
   end

   always begin
      # 10;
      a += 1;
   end
endmodule

module top;
   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic [7:0]          a;                      // From bug of bug.v
   // End of automatics

   bug bug(/*AUTOINST*/
           // Outputs
           .a                           (a[7:0]));

   initial begin
      # 1000;
      if (int'(a) > 5) begin
         $display("Test");
      end
   end

endmodule
// New File: <autologic_int.v>
interface ExampIf
  ( input logic clk );
   logic        req_val;
   logic [7:0]  req_dat;
   clocking mon_clkblk @(posedge clk);
      input 	req_val;
      input	req_dat;
   endclocking
   modport mp(clocking mon_clkblk);
endinterface

module ExampMain
( input clk,
  /*AUTOINOUTMODPORT("ExampIf" "mp")*/
  // Beginning of automatic in/out/inouts (from modport)
  input			req_val,
  input [7:0]		req_dat
  // End of automatics
);

   /*AUTOASSIGNMODPORT("ExampIf","mp","inst")*/
   // Beginning of automatic assignments from modport
   assign inst.req_dat = req_dat;
   assign inst.req_val = req_val;
   // End of automatics

endmodule
// New File: <automodport_ex.v>

module auto_module
( input my_clk,
  input 	my_rst_n,


  output 	manually_listed,

  /*AUTOINOUTMODPORT("automodport_if" "pure_mp")*/
  //ex: input	in_pure;
  //ex: output	out_pure;

  /*AUTOINOUTMODPORT("automodport_if" "req_mon_mp")*/
  //ex: input			req_credit,		// To auto_i of auto_intf.sv
  //ex: input [63:0]		req_data,		// To auto_i of auto_intf.sv
  //ex: input			req_val,		// To auto_i of auto_intf.sv

  /*AUTOINOUTMODPORT("automodport_if" "rsp_drv_mp")*/
  //ex: output [1:0]		rsp_cmd,		// From auto_i of auto_intf.sv
  //ex: input			rsp_credit,		// To auto_i of auto_intf.sv
  //ex: output [63:0]		rsp_data		// From auto_i of auto_intf.sv
);

   auto_intf auto_i
     (// Inputs
      .clk		(my_clk),
      .rst_n		(my_rst_n));

   /*AUTOASSIGNMODPORT("automodport_if", "req_mon_mp", "auto_i" )*/
   //ex: assign auto_i.req_credit         = req_credit;
   //ex: assign auto_i.req_data           = req_data;
   //ex: assign auto_i.req_val            = req_val;

   /*AUTOASSIGNMODPORT("automodport_if", "rsp_drv_mp", "auto_i" )*/
   //ex: assign rsp_cmd                   = auto_i.rsp_cmd;
   //ex: assign rsp_data                  = auto_i.rsp_data;
   //ex: assign auto_i.rsp_credit         = rsp_credit;

   /*AUTOASSIGNMODPORT("automodport_if", "r.*", "auto_i" )*/


   initial begin
      `cn_set_intf(virtual auto_intf.req_mon_mp, "auto_pkg::auto_intf", "req_mon_vi", auto_i.req_mon_mp );
      `cn_set_intf(virtual auto_intf.rsp_drv_mp, "auto_pkg::auto_intf", "rsp_drv_vi", auto_i.rsp_drv_mp );
   end

endmodule
// New File: <automodport_full.v>
interface automodport_if
( input logic clk,
  input logic rst_n
);

   //----------------------------------------------------------------------------------------
   // Group: Signals
   logic 		req_val;
   logic [63:0] 	req_dat;
   logic 		req_credit;

   logic [1:0] 		rsp_cmd;
   logic [63:0] 	rsp_data;
   logic 		rsp_credit;
   
   logic 		in_pure;
   logic 		out_pure;
   logic		manually_listed;

  //----------------------------------------------------------------------------------------
   // Group: Clocking blocks
   clocking req_mon_cb @(posedge clk);
      input 		rst_n;
      input 		req_val;
      input 		req_dat;
      input 		req_credit;
      input		manually_listed;
  endclocking : req_mon_cb

   clocking rsp_drv_cb @(posedge clk);
      input 		rst_n;
      output 		rsp_cmd;
      output 		rsp_data;
      input 		rsp_credit;
   endclocking : rsp_drv_cb

   //----------------------------------------------------------------------------------------
   // Group: Modports
   modport req_mon_mp(clocking req_mon_cb);
   modport rsp_drv_mp(clocking rsp_drv_cb, import rsp_reset);
   modport pure_mp(input in_pure, output out_pure);

   //----------------------------------------------------------------------------------------
   // Group: Methods
   function void rsp_reset();
      rsp_cmd = 2'b0;
      rsp_data = 64'b0;
   endfunction : rsp_reset

   //----------------------------------------------------------------------------------------
   // Group: Assertions
   logic [1:0] 		cmd_m1;

   always @(posedge clk) begin
      cmd_m1 <= rsp_cmd;
      if(rst_n) begin
         if($isunknown(req_val))                  `cn_err_intf(("Signal req_data_cycle is an X."));
         if(req_val==1'b1 & $isunknown(req_data)) `cn_err_intf(("Signal req_data is an X."));
         if($isunknown(req_credit))               `cn_err_intf(("Signal req_credit is an X."));

         if($isunknown(rsp_cmd))                  `cn_err_intf(("Signal rsp_cmd is an X."));
         if(cmd_m1!=2'b0 & $isunknown(rsp_data))  `cn_err_intf(("Signal rsp_data is an X."));
         if($isunknown(rsp_credit))               `cn_err_intf(("Signal rsp_credit is an X."));
      end
   end
   
endinterface
// New File: <automodport_if.v>
module bug1526;
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
   /*AUTOWIRE*/


   wire [3:0][27:0]        x;
   typedef wire [3:0][27:0] t;

   /*t2  AUTO_TEMPLATE  (
    .x(x),
    )*/
   t2 #(// Parameters
        .N(4)
        )
   t2 (/*AUTOINST*/
       // Outputs
       .x                               (x));                     // Templated

   /*t3  AUTO_TEMPLATE  (
    .x(t'(x)),
    )*/
   t3 #(// Parameters
        .N(4)
        )
   t3 (/*AUTOINST*/
       // Inputs
       .x                               (t'(x)));                 // Templated

endmodule

module t2 #(
            parameter N=4
            ) (
               output [N-1:0][27:0] x
               );
endmodule

module t3 #(
            parameter N=4
            ) (
               input [N-1:0][27:0] x
               );
endmodule
// New File: <autooutput_cast.v>
module top
  (input [(`WIDTH):0] a, /* This comma gets deleted */
   /*AUTOOUTPUT*/
   /*AUTOINPUT*/
   );
   
   child child(/*AUTOINST*/);
endmodule

module nocomma
  (/*AUTOOUTPUT*/
   /*AUTOINPUT*/
   );
   child child(/*AUTOINST*/);
endmodule

module ifdefcomma
  (
`ifdef a
   input foo,
`endif
   /*AUTOOUTPUT*/
   /*AUTOINPUT*/
   );
   child child(/*AUTOINST*/);
endmodule

module ifdefnocomma
  (
`ifdef a
   // It's up to the user to deal with the , themself
   input foo,
`endif
   /*AUTOOUTPUT*/
   );
   child child(/*AUTOINST*/);
endmodule

module child(input b);
endmodule
// New File: <autooutput_comma.v>
module ex_output_every (o,i,tempa,tempb)
  output o;
   input i;

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output		tempa;
   output		tempb;
   // End of automatics

   wire  tempa;
   wire  tempb;
   wire  o;

   assign tempa = i;
   assign tempb = tempa;
   assign o = tempb;
endmodule
// New File: <autooutputevery_assign.v>
module ex_output_every (o,i,tempa,tempb)
  output o;
   input i;

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output		tempa;
   output		tempb;
   // End of automatics

   wire  tempa = i;
   wire  tempb = tempa;
   wire  o = tempb;

endmodule
// New File: <autooutputevery_example.v>
module top
  (
   /*AUTOOUTPUTEVERY("^a")*/
   );   

   wire 		aa;
   wire 		ab;
   wire 		cc;
endmodule
// New File: <autooutputevery_regexp.v>
module test (/*AUTOARG*/
   // Outputs
   stage3_bus,
   // Inputs
   stage1_bus
   );

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output logic [7:0]	stage3_bus;		// From i_second of sub_module.v
   // End of automatics
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input logic [7:0]	stage1_bus;		// To i_first of sub_module.v
   // End of automatics
   /*AUTOOUTPUTEVERY("^stage")*/

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic [7:0]		stage2_bus;		// From i_first of sub_module.v
   // End of automatics
   /*AUTOREG*/

   /* sub_module AUTO_TEMPLATE 
            (
               .i_\(.*\)      (stage1_\1[]),
               .o_\(.*\)      (stage2_\1[]),
               );  */

   sub_module i_first (/*AUTOINST*/
		       // Outputs
		       .o_bus		(stage2_bus[7:0]),	 // Templated
		       // Inputs
		       .i_bus		(stage1_bus[7:0]));	 // Templated

   /* sub_module AUTO_TEMPLATE 
    (
               .i_\(.*\)      (stage2_\1[]),
               .o_\(.*\)      (stage3_\1[]),
               ); */

   sub_module i_second (/*AUTOINST*/
			// Outputs
			.o_bus		(stage3_bus[7:0]),	 // Templated
			// Inputs
			.i_bus		(stage2_bus[7:0]));	 // Templated

endmodule // test

module sub_module (/*AUTOARG*/
   // Outputs
   o_bus,
   // Inputs
   i_bus
   );

   input logic [7:0] i_bus ;
   output logic [7:0] o_bus ;

   assign o_bus = i_bus;

endmodule // sub_module
// New File: <autooutputevery_wire.v>
module ex_output_every (/*AUTOARG*/
   // Outputs
   sub1_out_pixel
   )

  /*AUTOOUTPUT*/
  // Beginning of automatic outputs (from unused autoinst outputs)
  output pixel24_t	sub1_out_pixel;		// From i1 of v2k_typedef_yee_sub1.v
  // End of automatics

  v2k_typedef_yee_sub1 i1
    (/*AUTOINST*/
     // Outputs
     .sub1_out_pixel			(sub1_out_pixel),
     .sub1_to_sub2			(sub1_to_sub2),
     .sub1_to_sub2_and_top		(sub1_to_sub2_and_top),
     // Inputs
     .sub1_in_pixel			(sub1_in_pixel),
     .cp				(cp),
     .reset				(reset));

endmodule

// Local Variables:
// verilog-auto-output-ignore-regexp:"^sub1_to_sub2"
// End:
// New File: <autooutput_regexp.v>
// msg2404

module top
  (
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/
   );

   /*AUTOLOGIC*/

   submod inst
     (/*AUTOINST*/);
    
endmodule

module submod
  (input [2*2:1] x);
endmodule

// Local Variables:
// verilog-auto-simplify-expressions: nil
// End:
// New File: <autooutput_simplify.v>
module autoreginput;

   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   reg			lower_ina;		// To inst of inst.v
   reg			lower_inb;		// To inst of inst.v
   // End of automatics
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire			lower_out;		// From inst of inst.v
   // End of automatics

   assign lower_ina = 1;

   inst inst (/*AUTOINST*/
	      // Outputs
	      .lower_out		(lower_out),
	      // Inputs
	      .lower_inb		(lower_inb),
	      .lower_ina		(lower_ina));

endmodule

// Local Variables:
// verilog-auto-reg-input-assigned-ignore-regexp: ".*"
// End:
// New File: <autoreginput_ignass.v>
module autoreginput;

   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   reg			lower_ina;		// To inst of inst.v
   reg			lower_inb;		// To inst of inst.v
   // End of automatics
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire			lower_out;		// From inst of inst.v
   // End of automatics

   inst inst (/*AUTOINST*/
	      // Outputs
	      .lower_out		(lower_out),
	      // Inputs
	      .lower_inb		(lower_inb),
	      .lower_ina		(lower_ina));

endmodule

// New File: <autoreginput.v>
module foo
  #(parameter
    PARAM3_P=99)
(
 output 		    sig3,// Here, I've hit "tab" twice and this line is correct
 output reg [PARAM2_P-1:0]  s4, // Comment here which should be at "comment-column"
 output wire [PARAM2_P-1:0] sig5,// Comment here which should be at "comment-column"
 input 			    reset_L// module reset: global register
 );

   /*AUTOWIRE*/

   // AUTOREG here is somewhat illegal, as outputs when a type is declared
   // in a V2K decl list are automatically "reg'ed".
   // However some simulators take it, so be sane and only do undeclared ones...

   /*AUTOREG*/

endmodule
// New File: <autoreg_outreg.v>
module test(/*AUTOARG*/
   // Outputs
   a1, b1, c1, one
   );

   output [5:0] a1;
   output [5:0] b1;
   output [5:0] c1;

   /*AUTOREG*/

   //wire [5:0]   a2,b2,c2;

   assign {a1,b1,c1} = {a2,b2,c2};

   output [1:0] one;
   assign one = a2[0];

endmodule
// New File: <autoreg_smith_multiassign.v>
module foo;

  reg [1:0] a;

  assert property (@(posedge clk) e |-> f);
    
  reg [1:0] b;

  always @(posedge clk, negedge rst)
    if (~rst) begin
      /*AUTORESET*/
      // Beginning of autoreset for uninitialized flops
      a <= 2'h0;
      b <= 2'h0;
      // End of automatics
    end
    else begin
      a<=b;
      b<=a;
    end

endmodule
// New File: <autoreset_assert.v>
module x;
    reg [1:0]        idx_sel_2a;
    always @(posedge usclk or negedge srst_0a_n) begin
      if (~srst_0a_n) begin
         /*AUTORESET*/
      end
      else begin
         foo <= idx_adr_1a[1:0];
         bar <= (idx_sel_2a == 2'h0 ? idx_rd_dat0_2a[12:0] :
                 idx_sel_2a == 2'h1 ? idx_rd_dat1_2a[12:0] :
                 idx_sel_2a == 2'h2 ? idx_rd_dat2_2a[12:0] :
                 /**/                 idx_rd_dat3_2a[12:0]);
      end
    end

endmodule
// New File: <autoreset_cond.v>
// bug594
`define TABADR_EGR_MOD_MAP 7:0

module test (
/*AUTOARG*/
   // Inputs
   clk, rst, addr
   );
   /*AUTOINPUT*/
   /*AUTOOUTPUT*/

   input clk;
   input rst;
   input[`TABADR_EGR_MOD_MAP] addr;


reg [`TABADR_EGR_MOD_MAP] addrD1;	
reg [`TABADR_EGR_MOD_MAP] addrD2;	

always @(posedge clk or posedge rst) begin
    if (rst_l) begin
           /*AUTORESET*/
	   // Beginning of autoreset for uninitialized flops
	   addrD1 <= `0/*NOWIDTH*/;
	   addrD2 <= `0/*NOWIDTH*/;
	   // End of automatics
     end
    else begin
       addrD1 <=  addr ;
       addrD2 <=  addrD1;
    end
end
endmodule
// New File: <autoreset_define_colon.v>
module x;
   always @(posedge piclk) begin
      if (k_i_reset) begin
	 /*AUTORESET*/
      end
      else begin
	 idat_ICErrData_i3 <= idat_way0_i2[1*OPCWID-1:0*OPCWID];
      end
   end

   // 2010-04-08
   localparam MPND = 5;
   always @(posedge usclk)
     if (~sso_srst_n) begin
	/*AUTORESET*/
     end
     else begin
	sel_s3 <= adr_s2[MIDX];
	rd_dat_s4 <= (sel_s3 == 1'h0 ? rd_dat0_s3[MPND:0]
		      :                rd_dat1_s3[MPND:0]);
     end

   // 2010-04-15
   integer i;
   always @(posedge usclk)
     if (~sso_srst_n) begin
	for (int j=0; j<10; j++) blob[j] <= 0;
	/*AUTORESET*/
     end
     else begin
	for (i=0; i<10; i++) blob[i] <= blob[i+1];
	for (i=0; i<10; i++) zz <= 1;
	for (int isv=0; isv<10; isv++) zsv <= 1;
     end

   always @(/*AS*/) begin
	for (i=0; i<10; i++) zz <= in;
	for (int isv=0; isv<10; isv++) zsv <= in;
   end

endmodule
// New File: <autoreset_dever.v>
module t();

   logic [31:0] sum;
   logic [31:0] a;
   logic [31:0] b;
   reg [32:0] tmp;

   always @(posedge clk or negedge rst) begin
      if (!rst) begin
	 /*AUTORESET*/
	 // Beginning of autoreset for uninitialized flops
	 sum <= 32'h0;
	 tmp = 33'h0;
	 // End of automatics
      end
      else begin
	 //reg [32:0] tmp;
	 tmp = a + b;
	 if (tmp[32])
	   sum <= 32'hffffffff;
	 else
	   sum <= tmp[31:0];
      end
   end

endmodule

// Local Variables:
// verilog-auto-reset-blocking-in-non: t
// End:
// New File: <autoreset_eq_bug381_non.v>
module t();

   logic [31:0] sum;
   logic [31:0] a;
   logic [31:0] b;
   reg [32:0] tmp;

   always @(posedge clk or negedge rst) begin
      if (!rst) begin
	 /*AUTORESET*/
	 // Beginning of autoreset for uninitialized flops
	 sum <= 32'h0;
	 // End of automatics
      end
      else begin
	 //reg [32:0] tmp;
	 tmp = a + b;
	 if (tmp[32])
	   sum <= 32'hffffffff;
	 else
	   sum <= tmp[31:0];
      end
   end

endmodule

// Local Variables:
// verilog-auto-reset-blocking-in-non: nil
// End:
// New File: <autoreset_eq_bug381.v>
module autoreset_equal_extra ();

   always_ff @(posedge csclk) begin
      if( ~srst_n | !val_tx_to_csi ) begin
	 csi.cmd <= ncbo_cmd_t'('0);
	 // vvvvvv   fifo.data.cb_req.req.cmd = '0;  should not be below
	 /*AUTORESET*/
      end
      else begin
	 if (fifo.sot) begin
	    csi.src <= fifo.src;
	    csi.wr  <= (fifo.data.cb_req.req.cmd == ncb_defs::NCBO_RSVD_LMTST
			| fifo.data.cb_req.req.cmd == ncb_defs::NCBO_IOBST
			);
	    csi.cmd <= fifo.data.cb_req.req.cmd;
	 end
      end
   end

   always_ff @(posedge csclk) begin
      if (~srst_n) begin
	 /*AUTORESET*/
      end
      else begin
	 sdp__x2p.err   <= (x2p_fifo_rdat.err & x2p_fifo_pop_d3_sk)
			   ? x2p_p2x_roc_rtl_pkg::X2P_PKT_ERR_RCVERR
			   : x2p_p2x_roc_rtl_pkg::X2P_PKT_ERR_NONE;
	 sdp__x2p.bval  <= x2p_fifo_rdat.bval & {4{x2p_fifo_pop_d3_sk}};
	 //FOO::bar <= 1;  // Legal, though not typically used in RTL
      end
   end
   
endmodule
// New File: <autoreset_equal_extra.v>
module x;
   always @(posedge clk or negedge reset_l) begin
      if (!reset_l) begin
         c <= 1;
         /*AUTORESET*/
      end
      else begin
         a <= in_a;
         b <= in_b;
         c <= in_c;
`ifndef DC
    x <= 1'b1;
`endif
      end
   end
endmodule
// New File: <autoreset_ifndef_dc.v>
//From: "Chris Kappler" <ckappler@hhnetwk.com>

//Verilint 440 off // WARNING: Clock is used as a reset
//Verilint 394 off // WARNING: Multiple clocks in the always block
//Verilint  71 off // WARNING: Case statement without default clause

module x (/*AUTOARG*/
   // Outputs
   a, b, c,
   // Inputs
   clk, reset_l, in_a, in_b, in_c
   );

   input clk, reset_l, in_a,in_b,in_c;
   output a,b,c;
   reg 	  a,b,c;

   always @(posedge clk or negedge reset_l) begin
      if (!reset_l) begin
         c <= 1;
         /*AUTORESET*/
	 // Beginning of autoreset for uninitialized flops
	 a <= #1 0;
	 b <= #1 0;
	 // End of automatics
      end
      else begin
         a <= in_a;
         b <= in_b;
         c <= in_c;
      end
   end

   always @(posedge clk or negedge reset_l) begin
      casex ({reset_l,in_a})
	2'b1_x: begin
           a <= in_a;
           b <= in_b;
           c <= in_c;
        end
	2'b0_x: begin
           c <= 1;
           /*AUTORESET*/
	   // Beginning of autoreset for uninitialized flops
	   a <= #1 0;
	   b <= #1 0;
	   // End of automatics
        end
      endcase
   end

   always @(/*AS*/in_a or in_b or reset_l) begin
      casex ({reset_l,in_a})
	2'b1_x: begin
           a = in_a;
           b = in_b;
        end
	2'b0_x: begin
           c = 1;
           /*AUTORESET*/
	   // Beginning of autoreset for uninitialized flops
	   a = 0;
	   b = 0;
	   // End of automatics
        end
      endcase
   end

endmodule

// Local Variables:
// verilog-auto-reset-widths: nil
// verilog-assignment-delay: "#1 "
// End:
// New File: <autoreset_if.v>
module aaa();
   always @(a) begin
      if (a) begin
         /*AUTORESET*/
      end
      // note missing e-n-d
   always @(*) begin
   end
endmodule
// New File: <autoreset_inf_bug325.v>
module autoreset_label_gorfajn;

   always_ff @(posedge clk or negedge reset)
     if (~reset) begin
       /*AUTORESET*/
     end else begin
       a<=b;
       foo: assert (b==4);
       bar: z<=1;
     end

endmodule
// New File: <autoreset_label_gorfajn.v>
//bug844
module device(
	      input 		 clk,
	      output logic [7:0] Q, out0,
	      output logic 	 pass, fail
	      input [7:0] 	 D, in0,in1 );

   enum logic [2:0] {IDLE, START, RUN, PASS, FAIL } state, next_state;
   logic 			 ready, next_ready;
   logic 			 next_pass, next_fail;

   always_ff @(posedge clk, negedge rstn)
     if (!rstn) begin
	state <= IDLE;
	/*AUTORESET*/
     end
     else begin
	state <= next_state;
	ready <= next_ready;
	pass <= next_pass;
	fail <= next_fail;
     end

   always @* begin
      if (!ready) begin
	 /*AUTORESET*/
      end
      else begin
	 out0 = sel ? in1 : in0;
      end
   end

   always_comb begin
      next_state = state;
      /*AUTORESET*/
      case (state)
	IDLE :	begin
	   // stuff ...
	end
	/* Other states */
	PASS:	begin
	   next_state = IDLE;
	   // stuff ...
	   next_pass = 1'b1;
	   next_ready = 1'b1;
	end
	FAIL:	begin
	   next_state = IDLE;
	   // stuff ...
	   next_fail = 1'b1;
	end
      endcase
   end

   always_latch begin
      if (!rstn) begin
	 /*AUTORESET*/
      end
      else if (clk) begin
	 Q <= D;
      end
   end
endmodule
// New File: <autoreset_latch.v>
module t;
   always_ff @(posedge csclk) begin
      if (~srst_n) begin
	 /*AUTORESET*/
      end
      else begin
	 if (dma_req_n2_sk) begin
	    foo.tgt <= '{attr  : in_attrib,
			       rsvd  : 2'b0,
			       lst   : in_last,
			       fst   : in_fst,
			       err   : in_err,
			       ring  : in_ring.vrg};
	    foo.ring   <= sg_cmd_n2_sk.vfr;
	    foo.length <= {3'h0,dma_size_n2_sk};
	    bar <= 1'b1;
	 end
	 else begin
	    foo.addr       <= nxt_dma_ptr_sk;
	 end
      end
   end
endmodule
// New File: <autoreset_pattern.v>
module x;
   reg a;
   reg b;
   reg c;
   always @(/*AUTOSENSE*/) begin
      if ( rst ) begin
	 /*AUTORESET*/
      end
      else begin
	 if ( a <= b ) begin
	    c = a;
	 end
	 else begin
	    c = b;
	 end
      end
   end

   always @ (posedge a) begin
      if ( rst ) begin
	 /*AUTORESET*/
      end
      else if ( a <= b ) begin
	 c <= a;
      end
   end

endmodule
// New File: <autoreset_reed.v>
module autoreset_regin;
   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   reg [7:0]		a;			// To bar of bar.v
   // End of automatics

   bar bar
     (
      /*AUTOINST*/
      // Inputs
      .a				(a[7:0]));

   always @(posedge clk) begin
      if (rst) begin
	 /*AUTORESET*/
	 // Beginning of autoreset for uninitialized flops
	 a <= 8'h0;
	 // End of automatics
      end
      else begin
	 a <= 8'h42;
      end
   end
endmodule

module bar
  input [7:0] a;
endmodule
// New File: <autoreset_regin.v>
//bug906

import gmcupkg::*;

module gminstdecode
  (
   output instClassType instClass
   /*blah blah blah*/);

   always_comb begin
      /*AUTORESET*/

      if (ldBitFromIo | stBitToIo) begin
	 instClass.isBool = 1'b1;
	 instClass.iFunc  = IFUNC_BOOL;
	 instClass.sub.bool = 1'b1;
	 instClass.sub2.sub3.bool = 1'b1;
      end
   end

   always_comb begin
      instClass = '{default:0};     // #1 (see below)
      /*AUTORESET*/

      if (ldBitFromIo | stBitToIo) begin
	 instClass.isBool = 1'b1;
	 instClass.iFunc  = IFUNC_BOOL;
      end
   end

   always_comb begin
      instClass.iFunc = IFUNC_ADD;  // #2
      /*AUTORESET*/

      if (ldBitFromIo | stBitToIo) begin
	 instClass.isBool = 1'b1;
	 instClass.iFunc  = IFUNC_BOOL;
      end
   end

   always_comb begin
      instClass.sub = '0;
      instClass.sub2 = '0;
      /*AUTORESET*/

      if (ldBitFromIo | stBitToIo) begin
	 instClass.sub.bool = 1'b1;
	 instClass.sub2.sub3.bool = 1'b1;
	 instClass.sub3.sub4.bool = 1'b1;
      end
   end
endmodule

// Local Variables:
// verilog-auto-reset-widths: unbased
// verilog-typedef-regexp: "Type$"
// End:
// New File: <autoreset_struct.v>
// $Revision: #70 $$Date: 2002/10/19 $$Author: wsnyder $ -*- Verilog -*-
//====================================================================

module CmpEng (/*AUTOARG*/);

   input clk;
   input reset_l;

   // ****************************************************************

   /*AUTOREG*/
   /*AUTOWIRE*/

   // ********* Prefetch FSM definitions ****************************

   reg [3:0] m_cac_state_r;

   reg [2:0] m_cac_sel_r, m_dat_sel_r, m_cac_rw_sel_r;

   reg	     m_wid1_r;
   reg [2:0] m_wid3_r;
   reg [5:2] m_wid4_r_l;

   logic [4:1] logic_four;

   logic [PARAM-1:0] paramed;

`define M 2
`define L 1
   parameter MS = 2;
   parameter LS = 1;

   reg [MS:LS] m_param_r;
   reg [`M:`L] m_def2_r;

   always @ (posedge clk) begin
      if (~reset_l) begin
	 m_cac_state_r <= CAC_IDLE;
	 m_cac_sel_r <= CSEL_PF;
	 /*AUTORESET*/
      end
      else begin
	 m_wid1_r <= 0;
	 m_wid3_r <= 0;
	 m_wid4_r_l <= 0;
	 m_param_r <= 0;
	 m_def2_r <= 0;
	 logic_four <= 4;
	 paramed <= 1;
      end
   end

endmodule

// Local Variables:
// verilog-auto-read-includes:t
// verilog-auto-sense-defines-constant: t
// verilog-auto-reset-widths: unbased
// verilog-active-low-regexp: "_l$"
// End:
// New File: <autoreset_widths_unbased.v>
// $Revision: #70 $$Date: 2002/10/19 $$Author: wsnyder $ -*- Verilog -*-
//====================================================================

module CmpEng (/*AUTOARG*/);

   input clk;
   input reset_l;

   // ****************************************************************

   /*AUTOREG*/
   /*AUTOWIRE*/

   // ********* Prefetch FSM definitions ****************************

   reg [3:0] m_cac_state_r;

   reg [2:0] m_cac_sel_r, m_dat_sel_r, m_cac_rw_sel_r;

   reg	     m_wid1_r;
   reg [2:0] m_wid3_r;
   reg [5:2] m_wid4_r_l;

   logic [4:1] logic_four;

   logic [PARAM-1:0] paramed;

`define M 2
`define L 1
   parameter MS = 2;
   parameter LS = 1;

   reg [MS:LS] m_param_r;
   reg [`M:`L] m_def2_r;

   always @ (posedge clk) begin
      if (~reset_l) begin
	 m_cac_state_r <= CAC_IDLE;
	 m_cac_sel_r <= CSEL_PF;
	 /*AUTORESET*/
      end
      else begin
	 m_wid1_r <= 0;
	 m_wid3_r <= 0;
	 m_wid4_r_l <= 0;
	 m_param_r <= 0;
	 m_def2_r <= 0;
	 logic_four <= 4;
	 paramed <= 1;
      end
   end

endmodule

// Local Variables:
// verilog-auto-read-includes:t
// verilog-auto-sense-defines-constant: t
// verilog-auto-reset-widths: t
// verilog-active-low-regexp: "_l$"
// End:
// New File: <autoreset_widths.v>
module dummy;

   parameter [TSTBITS-1:0] // synopsys enum tstate_info
		TIDLE = 3'b000,
		TCN1  = 3'b001,
		TCN2  = TCN1+1, // must be numbered consecutively
		TCN3  = TCN2+1,
		TCN4  = TCN3+1,
		TCN5  = TCN4+1,
		IOCLR = TCN5+1,
		TUNU1 = 3'b111;

   // state and output logic
   always @ (`ifdef SIMFSM
             ireset or
             `endif
             /*AUTOSENSE*/fooc2_qual or foocr_we or ioclrinst or tstate) begin
      ioclrtmout = 1'b0;
      case (tstate)
	TIDLE: begin
           if (foocr_we)
             ntstate = TCN1;
           else
             ntstate = TIDLE;
	end
	TCN1,TCN2,TCN3,TCN4,TCN5: begin
           if (ioclrinst | fooc2_qual)
             ntstate = TIDLE;
           else
             ntstate = tstate + 1;
	end
	IOCLR: begin
           ntstate = TIDLE;
           ioclrtmout = 1'b1;
	end
	TUNU1: begin
           ntstate = TIDLE;
	     `ifdef SIMFSM
           if (~ireset)
             $display("ERROR: entered unused state at %t",$time);
	     `endif
	end
	default: begin
           ntstate = 'bx;
           ioclrtmout = 1'bx;
	     `ifdef SIMFSM
           if (~ireset)
             $display("ERROR: entered unknown state at %t",$time);
	     `endif
	end
      endcase // case(tstate)
   end // always @ (...

endmodule // dummy
// New File: <autosense_aas_ifdef.v>
module autosense_or(/*AUTOARG*/
   // Outputs
   x, y,
   // Inputs
   a, c
   );

   input a;
   input c;
   output x;
   output y;

   always @(a/*AUTOSENSE*/) begin
      x = a;
   end
   always @(a/*AUTOSENSE*/ or c) begin
      x = a | c;
   end
   //   always @(a or/*AUTOSENSE*/c) begin
   //      x = a | c;
   //   end

endmodule

// New File: <autosense_add_or.v>
module test (/*AUTOARG*/
   // Outputs
   out1, out2,
   // Inputs
   sel, in0, in1, in2
   );

   input [1:0]     sel;
   input  	   in0, in1, in2;

   output 	   out1;
   output 	   out2;

   reg 		   out1;
   reg 		   out2;

   // Missing in2
   always @ (/*AS*/in0 or in1 or in2 or sel)
     if (sel == 2'b00)
       begin
	  out1 = in1;
	  out2 = in0;
       end
     else
       begin
	  out1 = in2;
	  out2 = in1;
       end

   // OK
   always @ (/*AS*/in1 or in2 or sel)
     if (sel == 2'b00)
       out1 = in1;
     else
       out1 = in2;

endmodule // test
// New File: <autosense_chadha.v>
//From: Anup Sharma <asharma@Eng.Sun.COM>

module x;

   always @ (/*AS*/`EC_EXCLU or `EC_SHARE or jpack_share) begin
      jb_mc_xact_state_a1[2:0] <= #1 jpack_share[7] ? `EC_SHARE : `EC_EXCLU;
   end
endmodule
// New File: <autosense_define_hang.v>
parameter sel_a = {1'b 0, 1'b 0}; // 2'd 0 works fine
parameter sel_b = {1'b 0, 1'b 1}; // 2'd 1 works fine
parameter sel_c = 2'd 2,
            sel_d = 2'd 3;

//parameter sel_a = {1'b 0, 1'b 0}, // 2'd 0 works fine
//	  sel_b = {1'b 0, 1'b 1}, // 2'd 1 works fine
//	  sel_c = 2'd 2,
//          sel_d = 2'd 3;
// New File: <autosense_dittrich_inc.v>
module testcase
  (/*AUTOARG*/
   // Outputs
   Z,
   // Inputs
   A, B, C, D, SEL
   );

   input       A, B, C, D;
   input [1:0] SEL;
   output      Z;

`include autosense_dittrich_inc.v

   always @(/*AS*/A or B or C or D or SEL) begin
      case (SEL)
        sel_a: Z = A;
        sel_b: Z = B;
        sel_c: Z = C;
        sel_d: Z = D;
        default: Z = D;
      endcase // case(SEL)
   end // always @ (...

endmodule // testcase

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autosense_dittrich.v>
module x (/*AUTOARG*/);

   // Which Internal Bank
`define DMC_AG_HADDR_BADDR_BST2_RNG 1:0 // Bank Address Range within Hexabyte Address for 2-Burst
`define DMC_AG_HADDR_BADDR_BST4_RNG 2:1 // Bank Address Range within Hexabyte Address for 4-Burst
`define DMC_AG_HADDR_BADDR_BST8_RNG 3:2 // Bank Address Range within Hexabyte Address for 8-Burst

   reg [NumBnks-1:0] ibnk_sel_s; // Muxed internal bank address subfield of
   command address
     always @(/*AUTOSENSE*/lio_buscfg_brstlen2_sr or lio_buscfg_brstlen4_sr or m_cdq_haddr_sr)
       begin : PeelIntBnkAddr
	  case ({lio_buscfg_brstlen4_sr,lio_buscfg_brstlen2_sr})
	    2'b01: // 2-burst
	      begin
		 ibnk_sel_s = m_cdq_haddr_sr[`DMC_AG_HADDR_BADDR_BST2_RNG];
	      end
	    2'b10: // 4-burst
	      begin
		 ibnk_sel_s = m_cdq_haddr_sr[`DMC_AG_HADDR_BADDR_BST4_RNG];
	      end
	    default: // 8-burst
	      begin
		 ibnk_sel_s = m_cdq_haddr_sr[`DMC_AG_HADDR_BADDR_BST8_RNG];
	      end
	  endcase
       end

endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autosense_gifford.v>
module x;

   always @ (/*AS*/a or b or c)
     if (a) q = b;
     else r = c;

   always @ (/*AS*/a or b or c or d or e)
     if (a) q = b;
     else if (c) r = d;
   /* skip comments as usual */
     else r = e;

endmodule
// New File: <autosense_if_else.v>
module autosense_inandout(a,b);

   input a;
   output b;
   reg 	  b;
   reg 	  c;

   always @(/*AUTOSENSE*/a) begin
      c=a;
      b=c;
   end

endmodule

// New File: <autosense_inandout.v>
parameter [2:0] PARAM_TWO = 2,
	     PARAM_THREE = 3;
parameter PARAM_FOUR = 4;
// New File: <autosense_inc_param.v>
`define LCH_GSWDATWID  3
`define COM_CLDATWID   2

module autosense_jbrown(/*AUTOARG*/
   // Outputs
   lch_gswdata,
   // Inputs
   com_cldata
   );

   output [`LCH_GSWDATWID-1:0]    lch_gswdata;    // data to switch
   input [`COM_CLDATWID-1:0] 	  com_cldata;     // data bus to clusters

   /*autowire*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics

   reg 				  tmp;
`define TEST_DEFINE   1'b0
   always @ (/*AUTOSENSE*/)
     begin
        /* AUTO_CONSTANT (`TEST_DEFINE) */
        tmp <= `TEST_DEFINE;  // test defines
     end

   // Local Variables:
   // verilog-library-directories:("." "../cdp/")
   // verilog-library-extensions:(".v" ".vh")
   // End:

endmodule
// New File: <autosense_jbrown.v>
module test
  #(parameter FOO=1)
    ( a, b );
input a;
input b;

reg   c;
wire  d;

always @(/*AUTOSENSE*/)
  begin
     c=0;
     if (d) c = b;
  end
endmodule
// New File: <autosense_lavinge.v>
module x;

   input [5:0] state;
   output [5:0] next;

   parameter 	CYCLEA = 1;
   parameter 	CYCLEC = 2;
   parameter 	MSTRA = 3;
   parameter 	MSTRB = 4;
   parameter 	MSTRC = 5;

   // make sure 'state' is listed
   always @ (/*AUTOSENSE*/done or nREQA or nREQB or nREQC or state) begin
      next = 6'b0;
      case (1'b1)
        state[CYCLEC] : begin
	   if (!nREQA && done)                         next[MSTRA] = 1'b1;
	   else if (!nREQB && nREQA && done)next[MSTRB] = 1'b1;
	   else if (!nREQC && nREQA && nREQB && done) next[MSTRC] = 1'b1;
	   else next[CYCLEC] = 1'b1;
	end
        state[MSTRA] : begin
	   if (!nREQB || !nREQC) next[CYCLEA] = 1'b1;
	   else                           next[MSTRA] = 1'b1;
	end
      endcase
   end
endmodule
// New File: <autosense_mcardle_onehot.v>
module x (/*AUTOARG*/
   // Outputs
   rmsk0, rmsk1, rmsk2,
   // Inputs
   endbyte0, endbyte1, endbyte2, strtbyte0, strtbyte1, strtbyte2
   );
   input endbyte0;
   input endbyte1;
   input endbyte2;
   input strtbyte0;
   input strtbyte1;
   input strtbyte2;
   output rmsk0;
   output rmsk1;
   output rmsk2;

   always @ (/*AS*/endbyte0 or strtbyte0) rmsk0 = maskcalc(strtbyte0,endbyte0);
   always @ (/*AS*/endbyte1 or strtbyte1) rmsk1 = maskcalc(strtbyte1,endbyte1);
   always @ (/*AS*/endbyte2 or strtbyte2) rmsk2 = maskcalc(strtbyte2,endbyte2);

endmodule
// New File: <autosense_metzger_space.v>
`define one 1'b1

// New File: <autosense_one.v>
module x;

   always @(/*AUTOSENSE*/arb_select_f or octet_idx) begin
      octet_flag[0] = |arb_select_f[ 7: 0];
      octet_flag[1] = |arb_select_f[15: 8];
      octet_flag[2] = |arb_select_f[23:16];
      octet_flag[3] = |arb_select_f[31:24];
      octet_flag[4] = |arb_select_f[39:32];
      octet_flag[5] = |arb_select_f[47:40];
      octet_flag[6] = |arb_select_f[55:48];
      octet_flag[7] = |arb_select_f[63:56];

      octet_available = |octet_flag;

      shifted8_64 = barrel_shifter(octet_flag, octet_idx[5:3]);
   end // always @ (arb_select_f)

endmodule

function [7:0] barrel_shifter;

      input [7:0] source;
      input [2:0] shift_amt;

      begin
	 case (shift_amt) //synopsys parallel_case full_case
           3'b0: barrel_shifter = source;
           3'b1: barrel_shifter = {source[0], source[7:1]};
           3'b2: barrel_shifter = {source[1:0], source[7:2]};
           3'b3: barrel_shifter = {source[2:0], source[7:3]};
           3'b4: barrel_shifter = {source[3:0], source[7:4]};
           3'b5: barrel_shifter = {source[4:0], source[7:5]};
           3'b6: barrel_shifter = {source[5:0], source[7:6]};
           3'b7: barrel_shifter = {source[6:0], source[7]};
	 endcase // case(shift_amt)
      end

endfunction // barrel_shifter

// New File: <autosense_peers_func.v>

module x;
   always @ (/*as*/a or ff)
     case (a)
       1: if (ff[3:0] == 4'd3)
         sadfsdff;
     endcase


endmodule
// New File: <autosense_rogoff.v>
module x;

  reg [31:0] 		   mb_reg_output;

  // 16 registers max, register 15 is always the version number, so 15 are useable
  // NOTE: number_interface_regs must be the maximum (or 16 in this case) to get the version register
  parameter 		   NUMBER_INTERFACE_REGS = 16;
  parameter 		   MB_REG_START = 3; // start at 4th register location 'h0010
  parameter 		   CMD_REG_0_ADDR = MB_REG_START;
  parameter 		   CMD_REG_1_ADDR = MB_REG_START+1;
  parameter 		   CMD_REG_2_ADDR = MB_REG_START+2;
  parameter 		   CMD_REG_3_ADDR = MB_REG_START+3;
  parameter 		   CMD_REG_4_ADDR = MB_REG_START+4;
  parameter 		   CMD_REG_5_ADDR = MB_REG_START+5;
  parameter 		   CMD_REG_6_ADDR = MB_REG_START+6;
  parameter 		   CMD_REG_7_ADDR = MB_REG_START+7;
  parameter 		   CMD_REG_8_ADDR = MB_REG_START+8;
  parameter 		   CMD_REG_9_ADDR = MB_REG_START+9;
  parameter 		   CMD_REG_10_ADDR = MB_REG_START+10;
  // mode regs
  parameter 		   MODE_REG_0_ADDR = CMD_REG_8_ADDR;

  // Absolute register 14 is Error counter
  parameter 		   CMD_REG_14_ADDR = 14;
  parameter 		   CRC_ERROR_REG_ADDR = CMD_REG_14_ADDR;
  // ------------ VERSION register is 15
  parameter 		   VERSION_REG_ADDR = 15;

  reg [NUMBER_INTERFACE_REGS-1:MB_REG_START]  mb_reg_wr;
  reg [NUMBER_INTERFACE_REGS-1:MB_REG_START]  mb_reg_rd;
  wire [31:0] 			   mb_reg_out_w [NUMBER_INTERFACE_REGS-1:MB_REG_START];
  wire [31:0] 			   interface_from_core_fp;

  assign 
	 mb_reg_out_w[VERSION_REG_ADDR] = BUILD_VERSION;

  integer     mb_loop;
  always @(/*AUTOSENSE*/)
    
    begin: MB_READ_WRITE_SEL_P

     mb_reg_wr = 'h0;
     mb_reg_rd = 'h0;
     mb_reg_output = interface_from_core_fp; 

     for(mb_loop = MB_REG_START; mb_loop < NUMBER_INTERFACE_REGS; mb_loop=mb_loop+1)
       begin
	if(mb_reg_select[mb_loop] == 1'b1)
	  begin
	   mb_reg_rd[mb_loop] 	= mb_reg_select[mb_loop] &  mb_reg_rwn;
	   mb_reg_wr[mb_loop] 	= mb_reg_select[mb_loop] & !mb_reg_rwn;
	   mb_reg_output 	= mb_reg_out_w[mb_loop];
	  end
       end
    end
endmodule // x
// New File: <autosense_smith.v>
module autosense (/*AUTOARG*/);

   input ina;
   input inb;
   input inc;
   output [1:0] out;
   output 	out2;

   /*AUTOREG*/

`include "autosense_inc_param.v"
`include "autosense_one.v"
`define Input ina

   always @(/*AUTOSENSE*/) begin
      case (inc)
	1'b1: out = {`Input ? `one : 1'b0, `Input};
	default: out = {2{inb}};
      endcase
   end


   always @(/*AUTOSENSE*/) begin
      out2 = `Input | PARAM_TWO | PARAM_THREE | PARAM_FOUR;
   end

   always @(*) begin
      // @ (/*AS*/)
      out3 = ina;
   end

   always @* begin
      out3 = ina;
   end

endmodule
// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <autosense.v>

module autosense_venkataramanan_begin(/*AUTOARG*/);

   reg a,b;

   always @(/*AUTOSENSE*/b or i) // I didn't expect to get "i" in AUTOSENSE
     begin : label
	integer i, j;
	for (i=0; i<= 3; i = i + 1)
	  vec[i] = b;
     end

endmodule

// Local Variables:
// verilog-auto-sense-defines-constant: t
// verilog-auto-sense-include-inputs: t
// End:
// New File: <autosense_venkataramanan_begin.v>
module test1 ( wire2, wire4 );
    output [3:0] wire2;
    output [16:0] wire4;
endmodule

// msg2099

module test_top;

    /*AUTOWIRE*/
    // Beginning of automatic wires (for undeclared instantiated-module outputs)
    wire		eq;			// From test_rcv of test1.v
    wire		not_eq;			// From test_foo of test1.v
    wire [16:0]		wire4;			// From test_rcv of test1.v, ...
    // End of automatics

   /*
     .wire2   (@"(if (equal \\"@\\" \\"rcv\\") \\"eq\\" \\"not_eq\\" )"),
    */

    /* test1 AUTO_TEMPLATE "test_\(.*\)" (
     .wire2\(\) (@"(if (equal \\"@\\" \\"rcv\\") \\"eq\\" \\"not_eq\\" )"),
    ); */

    test1 test_rcv (/*AUTOINST*/
		    // Outputs
		    .wire2		(eq),			 // Templated
		    .wire4		(wire4[16:0]));

    test1 test_foo (/*AUTOINST*/
		    // Outputs
		    .wire2		(not_eq),		 // Templated
		    .wire4		(wire4[16:0]));

endmodule
// New File: <autotemplate_lisp_eq.v>
module autotieoff_signed (/*AUTOARG*/);
   
   output [2:0]        ExtraOut;
   output [2:0]        SubOut;
   output [3:0]        active_low_l;
   output [3:0]        ignored_by_regexp;
   
   /*AUTOTIEOFF*/
   
endmodule

module sub;
   input  SubIn;
   output SubOut;
endmodule

// Local Variables:
// verilog-auto-tieoff-declaration: "assign"
// End:
// New File: <autotieoff_assign.v>
///bug1337

`ifdef N
module my_module
  #( parameter p = 1,
     parameter r = 2 )
   (
    output logic a,
    output logic b
    );
endmodule : my_module
`endif

module my_module_stub
  #(
    /*AUTOINOUTPARAM("my_module")*/
    // Beginning of automatic parameters (from specific module)
    parameter		p,
    parameter		r
    // End of automatics
    )
   (
    /*AUTOINOUTMODULE("my_module")*/
    // Beginning of automatic in/out/inouts (from specific module)
    output logic	a,
    output logic	b
    // End of automatics
    );

   /*AUTOTIEOFF*/
   // Beginning of automatic tieoffs (for this module's unterminated outputs)
   assign a					= 1'h0;
   assign b					= 1'h0;
   // End of automatics

endmodule : my_module_stub

// Local Variables:
// verilog-auto-tieoff-declaration: "assign"
// End:
// New File: <autotieoff_inoutmod.v>
module autotieoff_signed (/*AUTOARG*/);

   input [2:0]		ExtraIn;
   input [2:0]		SubIn;
   output [2:0] 	ExtraOut;
   output [2:0] 	SubOut;
   output [3:0] 	active_low_l;
   output [3:0] 	ignored_by_regexp;

   /*AUTOINOUTMODULE("autoinst_signed")*/

   // =============================
   // Auto Wires/Regs
   // =============================

   /*AUTOWIRE*/
   /*AUTOREG*/

   // =============================
   // Tieoffs
   // =============================

   /*AUTOTIEOFF*/

   // =============================
   
   sub sub (/*AUTOINST*/);

   // =============================
   // Unused signals
   // =============================

   // lint_checking SCX_UNUSED OFF
   wire _unused_ok = &{1'b0,
		       /*AUTOUNUSED*/
		       1'b0};
   // lint_checking SCX_UNUSED OFF

endmodule

module sub;
   input SubIn;
   output SubOut;
endmodule

// Local Variables:
// verilog-active-low-regexp: "_l$"
// verilog-auto-tieoff-ignore-regexp: "ignored"
// End:
// New File: <autotieoff_signed.v>
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2012 by Wilson Snyder.

module temp;

`define FOO(a)
`define BAR
`define BAZ
   
.....

`undef BAR
`define BAR
`define BAZ  // AGAIN
   
/*AUTOUNDEF*/

// Make sure this list is empty
/*AUTOUNDEF*/

//======================================================================

`define M_A
`define X_B

/*AUTOUNDEF("^M_")*/

endmodule
// New File: <autoundef.v>
module wbuf
  #(parameter width=1)
  (output [width-1:0] q,
   input [width-1:0]  d);
endmodule

module autounused
  #(parameter width=3)
  (/*AUTOARG*/) ;

   /*AUTOOUTPUT*/
   input    da, db, dc, dd;
   /*AUTOINPUT*/

   wire _unused_ok = &{/*AUTOUNUSED*/
                       1'b0};

   defparam partbuf.width = width;
   wbuf wbuf
     (// Inputs
      .d                                ({da,db,dc}),
      /*AUTOINST*/);

endmodule // abuf

// Local Variables:
// verilog-auto-unused-ignore-regexp: "^db$"
// End:
// New File: <autounused.v>
module FswArbiter (/*AUTOARG*/) ;


   // ========================
   //  Include parameter File
   // ========================
  parameter DMAO = 0;

`include "chip_fsw_spec_param.v"

   // ===============
   //  Physical Pins
   // ===============

   input	  sclk;				// Fabric switch clock
   input	  lreset_l;

   input          csr_insert_idle;
   input          csr_arb_enable;

   input [3:0] 	  csr_bypass3_enable;
   input [3:0] 	  csr_bypass2_enable;
   input [3:0] 	  csr_bypass1_enable;

   input          csr_xb_ecc_enable;

   output [3:0]   xb_ecc_error_dbit;
   output [3:0]	  xb_ecc_error_1bit;

   output         insert_idle_ack;

   output [2:0]   bp_performance_count;

   input         xb0_obx_ReqPst_s2a;
   input [3:0] 	 xb0_obx_NextVc_s2a;
   input [1:0] 	 xb0_obx_NextPort_s2a;
   input [3:0] 	 xb0_obx_NextXbe_s2a;
   input [3:0] 	 xb0_obx_NextXbe_s3a;
   input [71:0]  xb0_obx_OutDat_s4a;

   output        obx_xb0_GntPst_s3a;
   output	 obx_xb0_GntByp_s3a;

   input         xb0_obx_ReqBypS3_s2a;
   input         xb0_obx_ReqBypS2_s2a;

   input         xb1_obx_ReqPst_s2a;
   input [3:0] 	 xb1_obx_NextVc_s2a;
   input [1:0] 	 xb1_obx_NextPort_s2a;
   input [3:0] 	 xb1_obx_NextXbe_s2a;
   input [3:0] 	 xb1_obx_NextXbe_s3a;
   input [71:0]  xb1_obx_OutDat_s4a;

   output        obx_xb1_GntPst_s3a;
   output	 obx_xb1_GntByp_s3a;

   input         xb1_obx_ReqBypS3_s2a;
   input         xb1_obx_ReqBypS2_s2a;

   input         xb2_obx_ReqPst_s2a;
   input [3:0] 	 xb2_obx_NextVc_s2a;
   input [1:0] 	 xb2_obx_NextPort_s2a;
   input [3:0] 	 xb2_obx_NextXbe_s2a;
   input [3:0] 	 xb2_obx_NextXbe_s3a;
   input [71:0]  xb2_obx_OutDat_s4a;

   output        obx_xb2_GntPst_s3a;
   output	 obx_xb2_GntByp_s3a;

   input         xb2_obx_ReqBypS3_s2a;
   input         xb2_obx_ReqBypS2_s2a;

   input         xb3_obx_ReqPst_s2a;
   input [3:0] 	 xb3_obx_NextVc_s2a;
   input [1:0] 	 xb3_obx_NextPort_s2a;
   input [3:0] 	 xb3_obx_NextXbe_s2a;
   input [3:0] 	 xb3_obx_NextXbe_s3a;
   input [71:0]  xb3_obx_OutDat_s4a;

   output        obx_xb3_GntPst_s3a;
   output	 obx_xb3_GntByp_s3a;

   input         xb3_obx_ReqBypS3_s2a;
   input         xb3_obx_ReqBypS2_s2a;

   input         ib0_obx_PortSel_s1a;
   input [63:0]  ib0_obx_InDat_s1a;
   input [1:0]	 ib0_obx_NextPort_s1a;
   input [3:0]   ib0_obx_NextVc_s1a;

   input         ib1_obx_PortSel_s1a;
   input [63:0]  ib1_obx_InDat_s1a;
   input [1:0]	 ib1_obx_NextPort_s1a;
   input [3:0]   ib1_obx_NextVc_s1a;

   input         ib2_obx_PortSel_s1a;
   input [63:0]  ib2_obx_InDat_s1a;
   input [1:0]	 ib2_obx_NextPort_s1a;
   input [3:0]   ib2_obx_NextVc_s1a;

   input         ib3_obx_PortSel_s1a;
   input [63:0]  ib3_obx_InDat_s1a;
   input [1:0]	 ib3_obx_NextPort_s1a;
   input [3:0]   ib3_obx_NextVc_s1a;

   input         rp_is_full;
   input         rp_in_progress;

   input [15:0]  rp_arb_poolmask;
   input [63:0]  rp_arb_bufbusy_mask;

   output        xbx_grant_cycle;
   output        bp1_grant_cycle;
   output        set_arbitration_enable_d1;

   output [63:0] reset_ackbuf;
   output [63:0] wait_for_ack;

   output [63:0] ice9_databus;

   // =============================
   // Auto Wires/Regs
   // =============================

   /*AUTOWIRE*/
   /*AUTOREG*/

/* FswBypassArbiter AUTO_TEMPLATE (
			   // Outputs
			   .bp_grant3			(bp@_grant3),
			   .bp_grant2			(bp@_grant2),
			   .bp_grant1			(bp@_grant1),
			   .bp_grant0			(bp@_grant0),
			   .bp_grant_cycle		(bp@_grant_cycle),
			   .bp_grant_cycle_d1		(bp@_grant_cycle_d1),
			   .bp_grant_cycle_d2		(bp@_grant_cycle_d2),
			   .bp_next_winner		(bp@_next_winner[1:0]),
			   .bp_next_winner_d1		(bp@_next_winner_d1[1:0]),
			   .bp_next_winner_d2		(bp@_next_winner_d2[1:0]),
			   .bp_nextxbe			(bp@_nextxbe[3:0]),
			   .bp_nextxbe_d1		(bp@_nextxbe_d1[3:0]),
			   .bp_nextxbe_d2		(bp@_nextxbe_d2[3:0]),
			   .bp_hold_wait_vector		(bp@_hold_wait_vector[5:0]),
			   .bp_hold_wait_vector_d1	(bp@_hold_wait_vector_d1[5:0]),
			   .bp_select			(bp@_select),
			   .bp_select_d1		(bp@_select_d1),
			   .bp_select_d2		(bp@_select_d2),
			   .bp_header			(bp@_header),
			   .bp_header_d1		(bp@_header_d1),
			   .bp_header_d2		(bp@_header_d2),
			   // Inputs
			   .lreset_l			(bp@_lreset_l),
			   .bp_arb_enable		(bp@_arb_enable),
			   .sop3			(sop3 & xbx_bp3_enable_3),
			   .sop2			(sop2 & xbx_bp3_enable_2),
			   .sop1			(sop1 & xbx_bp3_enable_1),
			   .sop0			(sop0 & xbx_bp3_enable_0),
			   ); */


   FswBypassArbiter bp3 (/*AUTOINST*/);


/* FswBypassArbiter AUTO_TEMPLATE (
			   // Outputs
			   .bp_grant3			(bp@_grant3),
			   .bp_grant2			(bp@_grant2),
			   .bp_grant1			(bp@_grant1),
			   .bp_grant0			(bp@_grant0),
			   .bp_grant_cycle		(bp@_grant_cycle),
			   .bp_grant_cycle_d1		(bp@_grant_cycle_d1),
			   .bp_grant_cycle_d2		(bp@_grant_cycle_d2),
			   .bp_next_winner		(bp@_next_winner[1:0]),
			   .bp_next_winner_d1		(bp@_next_winner_d1[1:0]),
			   .bp_next_winner_d2		(bp@_next_winner_d2[1:0]),
			   .bp_nextxbe			(bp@_nextxbe[3:0]),
			   .bp_nextxbe_d1		(bp@_nextxbe_d1[3:0]),
			   .bp_nextxbe_d2		(bp@_nextxbe_d2[3:0]),
			   .bp_hold_wait_vector		(bp@_hold_wait_vector[5:0]),
			   .bp_hold_wait_vector_d1	(bp@_hold_wait_vector_d1[5:0]),
			   .bp_select			(bp@_select),
			   .bp_select_d1		(bp@_select_d1),
			   .bp_select_d2		(bp@_select_d2),
			   .bp_header			(bp@_header),
			   .bp_header_d1		(bp@_header_d1),
			   .bp_header_d2		(bp@_header_d2),
			   // Inputs
			   .lreset_l			(bp@_lreset_l),
			   .bp_arb_enable		(bp@_arb_enable),
			   .sop3			(sop3 & xbx_bp2_enable_3),
			   .sop2			(sop2 & xbx_bp2_enable_2),
			   .sop1			(sop1 & xbx_bp2_enable_1),
			   .sop0			(sop0 & xbx_bp2_enable_0),
			   ); */


   FswBypassArbiter bp2 (/*AUTOINST*/);


/* FswBypassArbiter AUTO_TEMPLATE (
			   // Outputs
			   .bp_grant3			(bp@_grant3),
			   .bp_grant2			(bp@_grant2),
			   .bp_grant1			(bp@_grant1),
			   .bp_grant0			(bp@_grant0),
			   .bp_grant_cycle		(bp@_grant_cycle),
			   .bp_grant_cycle_d1		(bp@_grant_cycle_d1),
			   .bp_grant_cycle_d2		(bp@_grant_cycle_d2),
			   .bp_next_winner		(bp@_next_winner[1:0]),
			   .bp_next_winner_d1		(bp@_next_winner_d1[1:0]),
			   .bp_next_winner_d2		(bp@_next_winner_d2[1:0]),
			   .bp_nextxbe			(bp@_nextxbe[3:0]),
			   .bp_nextxbe_d1		(bp@_nextxbe_d1[3:0]),
			   .bp_nextxbe_d2		(bp@_nextxbe_d2[3:0]),
			   .bp_hold_wait_vector		(bp@_hold_wait_vector[5:0]),
			   .bp_hold_wait_vector_d1	(bp@_hold_wait_vector_d1[5:0]),
			   .bp_select			(bp@_select),
			   .bp_select_d1		(bp@_select_d1),
			   .bp_select_d2		(bp@_select_d2),
			   .bp_header			(bp@_header),
			   .bp_header_d1		(bp@_header_d1),
			   .bp_header_d2		(bp@_header_d2),
			   // Inputs
			   .lreset_l			(bp@_lreset_l),
			   .bp_arb_enable		(bp@_arb_enable),
			   .sop3			(sop3 & csr_bp1_enable_3),
			   .sop2			(sop2 & csr_bp1_enable_2),
			   .sop1			(sop1 & csr_bp1_enable_1),
			   .sop0			(sop0 & csr_bp1_enable_0),
			   ); */


   FswBypassArbiter bp1 (/*AUTOINST*/);


   // =======================================
   // Coverage
   // =======================================

   // psl default clock = negedge sclk;
   generate if (DMAO == 0)
     begin
   // psl cover {lreset_l & (bp1_grant0 |bp1_grant1 |bp1_grant2 |bp1_grant3 )} report "FswPerfRtl::byp1Taken";
   // psl cover {lreset_l & (bp2_grant0 |bp2_grant1 |bp2_grant2 |bp2_grant3)} report "FswPerfRtl::byp2Taken";
   // psl cover {lreset_l & (bp3_grant0 |bp3_grant1 |bp3_grant2 |bp3_grant3)} report "FswPerfRtl::byp3Taken";
     end
   endgenerate

   // ================
   //  Unused signals
   // ================

   // lint_checking SCX_UNUSED off
   wire _unused_ok = &{1'b0,

		       bp_select,

		       bp3_hold_wait_vector,
		       bp2_hold_wait_vector_d1,
		       bp1_hold_wait_vector_d1,

		       bp3_header,
		       bp3_header_d1,
		       bp3_select,
		       bp3_select_d1,
		       bp3_next_winner[1:0],
		       bp3_next_winner_d1[1:0],
		       bp3_nextxbe[3:0],
		       bp3_nextxbe_d1[3:0],


		       bp2_grant_cycle_d2,
		       bp2_header,
		       bp2_header_d2,
		       bp2_select,
		       bp2_select_d2,
		       bp2_next_winner[1:0],
		       bp2_next_winner_d2[1:0],
		       bp2_nextxbe[3:0],
		       bp2_nextxbe_d2[3:0],

		       bp1_header_d1,
		       bp1_header_d2,
		       bp1_select_d1,
		       bp1_select_d2,
		       bp1_next_winner_d1[1:0],
		       bp1_next_winner_d2[1:0],
		       bp1_nextxbe_d1[3:0],
		       bp1_nextxbe_d2[3:0],

		       xb0_obx_NextXbe_s3a,	// This is unused signal now.
		       xb1_obx_NextXbe_s3a,
		       xb2_obx_NextXbe_s3a,
		       xb3_obx_NextXbe_s3a,

		       syn64,
		       dataout64,

		       1'b0
		       };
   // lint_checking SCX_UNUSED on


endmodule

module FswBypassArbiter (/*AUTOARG*/) ;

   input        lreset_l;
   input	sclk;
   input	bp_arb_enable;
   output       bp_grant3;
   output       bp_grant2;
   output       bp_grant1;
   output       bp_grant0;
   output  	bp_grant_cycle;
   output 	bp_grant_cycle_d1;
   output 	bp_grant_cycle_d2;
   output [1:0] bp_next_winner;
   output [1:0]	bp_next_winner_d1;
   output [1:0]	bp_next_winner_d2;
   output [3:0] bp_nextxbe;
   output [3:0] bp_nextxbe_d1;
   output [3:0] bp_nextxbe_d2;
   output [5:0]	bp_hold_wait_vector;
   output [5:0]	bp_hold_wait_vector_d1;
   output	bp_header;
   output	bp_header_d1;
   output	bp_header_d2;
   output	bp_select;
   output	bp_select_d1;
   output	bp_select_d2;
   input 	sop3;
   input [63:0] sop3_data;
   input [5:0]  sop3_bpcontext;
   input 	sop2;
   input [63:0] sop2_data;
   input [5:0]  sop2_bpcontext;
   input 	sop1;
   input [63:0] sop1_data;
   input [5:0]  sop1_bpcontext;
   input 	sop0;
   input [63:0] sop0_data;
   input [5:0]  sop0_bpcontext;
   input [15:0] poolmask;
   input [63:0] bufbusy_mask;
endmodule
// New File: <autowire_godiwala.v>
package pkg;
  typedef logic [1:0] my_type;
endpackage

module top;
  import pkg::*;
  /*AUTOWIRE*/
  sub_1 sub_1 (.*);
  sub_2 sub_2 (.*);
endmodule

module sub_1
  import pkg::*; // bug317
  (
   input  pkg::my_type_t a,
   output pkg::my_type_t z
   );
endmodule

module sub_2
  (
   input  pkg::my_type_t z,
   output pkg::my_type_t a
   );
endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-auto-star-save: t
// End:
// New File: <autowire_import_bug317.v>
module t;
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire			w11;			// From bp3 of FswBypassArbiter.v
   wire			w12;			// From bp3 of FswBypassArbiter.v
   // End of automatics
   /*AUTOREG*/

   wand 		w0;
   wor 			w1;
   tri0 		w2;
   tri1 		w3;
   triand		w4;
   trior		w5;
   trireg		w6;
   tri			w7;
   wire			w8;
   supply0		w9;
   supply1		w10;

   FswBypassArbiter bp3 (/*AUTOINST*/
			 // Outputs
			 .w0			(w0),
			 .w1			(w1),
			 .w2			(w2),
			 .w3			(w3),
			 .w4			(w4),
			 .w5			(w5),
			 .w6			(w6),
			 .w7			(w7),
			 .w8			(w8),
			 .w9			(w9),
			 .w10			(w10),
			 .w11			(w11),
			 .w12			(w12));

endmodule

module FswBypassArbiter (/*AUTOARG*/
   // Outputs
   w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12
   ) ;
   output       w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12;
endmodule
// New File: <autowire_isaacson.v>
module autowire_long_yaohung(/*AUTOARG*/);
   
   /*AUTOOUTPUT*/
   /*AUTOWIRE*/
   /*AUTOREG*/
   
   top top
     (/*AUTOINST*/);
   
endmodule

module top(/*AUTOARG*/);
   
   output [`LONGNAMELONGNAMELONGNAMELONGNAMELONGNAMELONGNAME] data_out;
   
endmodule
// New File: <autowire_long_yaohung.v>
module foo
  (/*AUTOARG*/
   // Outputs
   another_output2
   );

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output [1:0]		another_output2;	// From inst of autoinst_signed_fubar2.v
   // End of automatics

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   wire [1:0]		another_output2;	// From inst of autoinst_signed_fubar2.v
   // End of automatics


   /* autoinst_signed_fubar2 AUTO_TEMPLATE (
    .an_output2	(hi.ear.ial),
    );
    */

   autoinst_signed_fubar2 inst
     (
      /*AUTOINST*/
      // Outputs
      .an_output2			(hi.ear.ial),		 // Templated
      .another_output2			(another_output2[1:0]),
      // Inputs
      .an_input2			(an_input2[1:0]));

endmodule
// New File: <autowire_lovell_hiear.v>
module TOP (/*AUTOARG*/
   // Outputs
   SIG_NAMEB, SIG_NAMEA
   );
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output [223:0]	SIG_NAMEA;		// From A of A.v, ...
   output [FOO*2:0]	SIG_NAMEB;		// From C of C.v
   // End of automatics
   /*AUTOINPUT*/
   /*AUTOWIRE*/

   A A(/*AUTOINST*/
       // Outputs
       .SIG_NAMEA			(SIG_NAMEA[224*1-1:128*1]));
   B B(/*AUTOINST*/
       // Outputs
       .SIG_NAMEA			(SIG_NAMEA[127:0]));
   C C(/*AUTOINST*/
       // Outputs
       .SIG_NAMEB			(SIG_NAMEB[FOO*4-2*1:0]));
endmodule

module A(/*AUTOARG*/
   // Outputs
   SIG_NAMEA
   );
   output [224*1-1:128*1] SIG_NAMEA;
   //output [223:128] SIG_NAMEA;
endmodule


module B(/*AUTOARG*/
   // Outputs
   SIG_NAMEA
   );
   output [127:0] SIG_NAMEA;
endmodule


module C(/*AUTOARG*/
   // Outputs
   SIG_NAMEB
   );
   output [FOO*4-2*1:0] SIG_NAMEB;
endmodule


// New File: <autowire_merge_bug303.v>
interface my_interface ();
   logic [2:0] out2;
   logic [2:0] out3;
endinterface: my_interface

module foobar (input [2:0] in2, output [2:0] out2);
endmodule

module foo_autowire_fails (my_interface itf);
   /*AUTOWIRE*/
   assign itf.out2 = out2; // perhaps a namespace collision?
   foobar foobar0
     (/*AUTOINST*/);
endmodule   

module foo_autowire_works (my_interface itf);
   /*AUTOWIRE*/
   assign itf.out3 = out2;
   foobar foobar0
     (/*AUTOINST*/);
endmodule
// New File: <autowire_misalias_bug295.v>
module test1 ( wireA, wireB );
    output [3:0] wireA;
    output [16:0] wireB;
endmodule

module test_top;

   /*AUTOWIRE*/

   generate
      if (1) begin
	 test1 t1 (/*AUTOINST*/);
      end
      else begin
	 assign wireA = 0;
      end
   endgenerate
endmodule
// New File: <autowire_myers.v>
module top;

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output logic		out1a;
   output logic [1:0]	out1b;
   // End of automatics
   /*AUTOREG*/

   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic		out1a;
   logic [1:0]		out1b;
   // End of automatics


   sub2 isub2 (/*AUTOINST*/
	       // Outputs
	       .out1a			(out1a),
	       .out1b			(out1b[1:0]),
	       // Inputs
	       .in1a			(in1a),
	       .in1b			(in1b));

endmodule

module sub2
  ( input logic in1a,
    input logic in1b,
    output logic out1a,
    output logic [1:0] out1b
    );
endmodule

// Local Variables:
// verilog-auto-wire-comment: nil
// End:
// New File: <autowire_nocomment.v>
module Abc_TEST ();

/*AUTOWIRE*/

  Abc #(
        .No1                            (6),
        /*AUTOINSTPARAM*/) u_Abc
  (
   /*AUTOINST*/);

  Def #(
        .No1                            (6)) u_Def
  (
   // Outputs
   .ck                                  (ck),
    /*AUTOINST*/);

endmodule   


module Abc
  #(
  parameter No1 = 6,
  parameter int unsigned No2                                                  // Parameter no. 2
                       = pa_Abc::No2,
  parameter bit          No3 [No1:0][No2-1:0]                                 // Parameter no. 3
                       = pa_Abc::No3
  )
  (
  input  logic ck,
  input  logic [No1-1:0][31:0] abc
  input  logic [No1-1:0][31:0] abc
  );
endmodule   

module Def
  #(
  parameter No1 = 6
  )
  (
  input  logic ck,
  output  logic [No1-1:0][31:0] abc
  );
endmodule   

// Local Variables:
// verilog-library-extensions:(".v" ".sv")
// verilog-auto-inst-param-value:t
// End:
// New File: <autowire_paramvec_bug302.v>
`default_nettype none

package testcase_pkg;

  typedef int unsigned uint;

  localparam uint SIZE = 8;

  typedef enum {ENUM1, ENUM2} enum_t;

endpackage

module testcase_top
  (
   input  testcase_pkg::enum_t top_enum,
   input  logic [testcase_pkg::SIZE-1:0] top_in,
   output logic [testcase_pkg::SIZE-1:0] top_out
   );
  import testcase_pkg::*;
  //enum_t sub_enum; // is not declared by AUTOWIRE
  /*AUTOWIRE*/
  // Beginning of automatic wires (for undeclared instantiated-module outputs)
  testcase_pkg::enum_t	sub_enum;		// From testcase_sub1 of testcase_sub1.v
  wire [testcase_pkg::SIZE-1:0] sub_in;		// From testcase_sub1 of testcase_sub1.v
  wire [testcase_pkg::SIZE-1:0] sub_out;	// From testcase_sub2 of testcase_sub2.v
  // End of automatics
  assign top_out  = sub_out;
  testcase_sub1 testcase_sub1 (.*);
  testcase_sub2 testcase_sub2 (.*);
endmodule

module testcase_sub1
  (
   input  testcase_pkg::enum_t top_enum,
   output testcase_pkg::enum_t sub_enum,
   output logic [testcase_pkg::SIZE-1:0] sub_in
   );
  import testcase_pkg::*;
  assign sub_enum = top_enum;
  assign sub_in = '1;
endmodule

module testcase_sub2
  (
   input  testcase_pkg::enum_t sub_enum,
   input  logic [testcase_pkg::SIZE-1:0] sub_in,
   output logic [testcase_pkg::SIZE-1:0] sub_out
   );
  import testcase_pkg::*;
  assign sub_out = (sub_enum==ENUM1) ? ~sub_in : sub_in;
endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-auto-star-save: t
// End:
// New File: <autowire_pkg_bug195.v>
module sc_top (
  input var real  Tx_vcm,
  input var real  i_DAC_in,
  input           i_Tx_SC_en,
  output var real Tx_vsc
);

endmodule


module cm_top (
  input           i_Tx_CM_en,
  output var real Tx_vcm
);

endmodule

module top (
/*AUTOOUTPUT*/
/*AUTOINPUT*/
);

/*AUTOWIRE*/

cm_top cm_buf (/*AUTOINST*/);

sc_top sc_buf (/*AUTOINST*/);

endmodule
// New File: <autowire_real.v>

module top
  #(
    parameter  NOF_TEST        = 6,
    parameter integer TEST  [NOF_TEST] = '{10,20,30,40,50,60}
    )

   (
    /*AUTOINPUT*/
    /*AUTOOUTPUT*/
    // Beginning of automatic outputs (from unused autoinst outputs)
    output [(TEST[1])-1:0] x			// From inst of submod.v
    // End of automatics
    );

   /*submod  AUTO_TEMPLATE (
    .NUM_MEM        (TEST[1]),
    );
    */

   submod #( /*AUTOINSTPARAM*/
	    // Parameters
	    .NUM_MEM			(TEST[1]))		 // Templated
   inst
     (
      /*AUTOINST*/
      // Outputs
      .x				(x[(TEST[1])-1:0]));

endmodule

module submod
  #(parameter  NUM_MEM        = 6
    )
   (output [NUM_MEM-1:0] x);
endmodule

// Local Variables:
// verilog-auto-inst-param-value:t
// End:
// New File: <autowire_red_bracket.v>
`include "pf_sc.vh"

module autowire_req_sw
(
 input reqcmd_t AReq,
 output reqcmd_t BReq
/*AUTOINPUT*/
);

assign Bnk0Req =  Cpu0Req;


endmodule


// Local Variables:
// verilog-library-directories:(".")
// verilog-typedef-regexp:"_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <autowire_req_sw.v>

module autowire_req ();

reqcmd_t AReq;
reqcmd_t BReq;

/*AUTOWIRE*/

autowire_req_sw  autowire_req_sw (/*AUTOINST*/
				  // Outputs
				  .BReq			(BReq),
				  // Inputs
				  .AReq			(AReq));

endmodule

// Local Variables:
// verilog-library-directories:(".")
// verilog-typedef-regexp:"_t$"
// End:
// New File: <autowire_req.v>
// verilog mode bug1346 testcase
module design(/*AUTOARG*/);
   parameter w1 = 2;
   parameter w2 = 4;
   parameter w3 = 256;
   input [w1:0]       i0;
   input [w2:0]       i1;

   output [w2*w1  :0] y10; // 8:0
   output [w2/w1  :0] y11; // 2:0
   output [w2+w1  :0] y12; // 6:0
   output [w2-w1  :0] y13; // 2:0
   output [w2>>w1 :0] y14; // 1:0
   output [w2>>>w1:0] y15; // 1:0
   output [w2<<w1 :0] y16; //16:0
   output [w2<<<w1:0] y17; //16:0
   output [w2<>>w1:0] y18; //4<>>2:0
   output [w2>><w1:0] y19; //4>><2:0

   output [w1*w2/w1+w1            :0] y20; // 6:0
   output [w2*w1/w1+w1            :0] y21; // 6:0
   output [w1*w2/(w2-w1)+w1       :0] y22; // 6:0
   output [w2*w1/(w2-w1)+w2-w1    :0] y23; // 6:0
   output [w2*w1/(w2-w1)+w1<<1>>1 :0] y24; // 6:0
   output [w2*w1/(w2-w1)+w1<<w1-w1:0] y25; // 6:0
   output [(8*4)-1                :0] y26; // 31:0
   output [((w3>>3)-1)            :0] y27; // 31:0

   output [w2*w1/w1      +w2+w1 <<w2    >>w1   :0] y30; // 40:0
   output [w2*w1/w1      +w1+w2 <<w1+w1 >>w2-w1:0] y31; // 40:0
   output [w2*w1/w1      +w1+w2 <<w1+w1 >>w2/w1:0] y32; // 40:0
   output [w1*w2/w1      +w1+w2 <<w2    >>w1   :0] y33; // 40:0
   output [w1*w2/(w2-w1) +w1+w2 <<w2    >>w1   :0] y34; // 40:0
   output [w1*w2/(w2/w1) +w1+w2 <<w2    >>w2-w1:0] y35; // 40:0
   output [w1*w2/(w1+0)  +w1+w2 <<w2    >>w1   :0] y36; // 40:0

   output [w2*w1/w1      +w2*1+w1 <<w2/1   *1 >>w1   *1:0] y40; // 40:0
   output [w2*w1/w1      +w1*1+w2 <<w1/1+w1*1 >>w2-w1*1:0] y41; // 40:0
   output [w2*w1/w1      +w1*1+w2 <<w1/1+w1*1 >>w2/w1*1:0] y42; // 40:0
   output [w1*w2/w1      +w1*1+w2 <<w2/1   *1 >>w1   *1:0] y43; // 40:0
   output [w1*w2/(w2-w1) +w1*1+w2 <<w2/1   *1 >>w1   *1:0] y44; // 40:0
   output [w1*w2/(w2/w1) +w1*1+w2 <<w2/1   *1 >>w2-w1*1:0] y45; // 40:0
endmodule // design

module test(/*AUTOARG*/);

   /*AUTOINPUT*/

   /*AUTOOUTPUT*/

   design #(.w1(2),.w2(4),.w3(256)) i0_design(/*AUTOINST*/);

endmodule // test

// Local Variables:
// verilog-auto-inst-param-value: t
// End:
// New File: <autowire_shifts_bug1346.v>
module foo
  (/*AUTOARG*/);

   /*AUTOOUTPUTEVERY*/

   /*AUTOWIRE*/


   // Check that if aa is connected differently in a input, it doesn't make conflicts.

   autoinst_signed_fubar2 inst
     (
      // Outputs
      .an_output2			(hi.ear.ial),
      .another_output2			(aa[FOO:0]),
      // Inputs
      .an_input2			(an_input2[1:0])
      /*AUTOINST*/);

   autoinst_signed_fubar2 instx
     (
      // Outputs
      .an_output2			(hi.ear.ial),
      // Inputs
      .an_input2			(an_input2[1:0]),
      .another_output2			(aa[Y:X]),
      /*AUTOINST*/);

endmodule
// New File: <autowire_thon_selects.v>
module autowire_topv_one;
   output [1:0] foo;
   output [1:0] bar;
endmodule
// New File: <autowire_topv_one.v>
module autowire_topv;
   /*AUTOOUTPUT("bar")*/

   /*AUTOWIRE*/

   autowire_topv_one one (/*AUTOINST*/);
   autowire_topv_two two (/*AUTOINST*/);
endmodule

// Local Variables:
// verilog-auto-wire-type: "wire"
// End:
// New File: <autowire_topv.v>
module test1 ( wire2, wire4 );
    output [3:0] wire2;
    output [16:0] wire4;
endmodule

module test2 ( wire6, wire12 );
    input [3:0] wire6;
    input [16:0] wire12;
endmodule

module test3 ( wireA, wireB );
    input [3:0] wireA;
    input [16:0] wireB;
endmodule

module test4 ( wireA, wireB );
    output [3:0] wireA;
    output [16:0] wireB;
endmodule

module test_top;

    /*AUTOWIRE*/

    /* test1 AUTO_TEMPLATE (
        .wire@(wire_\1_to_@"(* \1 3)"[]),
    ); */

    test1 t1 (/*AUTOINST*/);

    /* test2 AUTO_TEMPLATE (
        .wire@(wire@"(/ \1 3)"_to_\1[]),
    ); */

    test2 t2 (/*AUTOINST*/);

    test3 t3 (/*AUTOINST*/);

    test4 t4 (/*AUTOINST*/);

endmodule
// New File: <autowire_totte.v>
module test
  (/*AUTOARG*/);

   parameter WIDTH = 16;
  
   // ISSUE: in autowire below [WIDTH*0-1:0] should have been [WIDTH*2/8-1:0]

   /*AUTOWIRE*/

   subtestin #(.WIDTH(WIDTH)) subtestin
     (/*AUTOINST*/);

   
   subtestout #(.WIDTH(WIDTH)) subtestout
     (/*AUTOINST*/);

endmodule

module subtestin
  #(parameter WIDTH=0)
  (input wire [WIDTH*2/8-1:0] signal
   /*AUTOARG*/);
endmodule

module subtestout
  #(parameter WIDTH=0)
  (output wire [WIDTH*2/8-1:0] signal
   /*AUTOARG*/);
   assign signal = 0;
endmodule
// New File: <autowire_width_muldiv.v>
module batch_li_child
#(parameter

  WIDTH_0= 'h8,
  WIDTH_1 = 'h4
  )
(
 input rst,
 input clk
 );

   reg [WIDTH_0-1:0] counter_0;
   reg [WIDTH_1-1:0] counter_1;

   always @(posedge clk) begin
      if (rst) begin
	 counter_0 <= #1 0;
	 counter_1 <= #1 0;
      end
      else begin
	 counter_0 <= #1 counter_0 + 1'b1;
	 counter_1 <= #1 counter_1 + 1'b1;	 
      end
   end

endmodule

// New File: <batch_li_child.v>
module batch_li_parent (/*AUTOARG*/);

   input rst;
   input clk;

   parameter WIDTH_0 = 8;
   parameter WIDTH_1 = 16;
   
   batch_li_child
     #(.WIDTH_1 (WIDTH_0),
       .WIDTH_0 (WIDTH_1))
     child
       (/*AUTOINST*/);

endmodule

// Local Variables:
// verilog-auto-arg-sort: t
// End:
// New File: <batch_li_parent.v>
module batch_prof_cell(Z,A,B);
  output  Z;
  input   A, B;
endmodule
// New File: <batch_prof_cell.v>
module x ();
   always @ (/*AUTOSENSE*/bus or in1 or in2 or reset) begin
      out4 = | bus;

      out7 = 1'b0; // pre-initialize output of case so default not needed
      case (bus[1:0])
        2'b00: out7 = in1;
        2'b01: out7 = in2;
        2'b10: out7 = reset;
`ifdef BEH
        default: begin
           out7 = 1'bX; // force VCS simulation to propagate X's from the bus signal
           $display ("\n Error, in module temp, bus[1:0] illegal value of 11\n");
        end
`endif

      endcase // case(bus[1:0])
   end
endmodule
// New File: <carlson.v>
module test ();

   always @(/*AUTOSENSE*/xyz)
     begin
	casex (xyz)
          4'b???0: r = 1;
          4'b??01: r = 2;
          4'b?001: r = 3;
          default: r = 4;
	endcase
     end

   assign x = y;

endmodule

// New File: <case_question.v>
module foo;
   initial begin
      /* The function verilog-strip-comments should not change this line:*/
      $display("INFO :[PIPE LINK %d]: ///////////////////////////////////////////",PIPE_LINK);
      
      // to this:
      $display("INFO :[PIPE LINK %d]: ");

      /* The function verilog-strip-comments should not change this line:*/
      $display("INFO :[PIPE LINK %d]: /* ",PIPE_LINK); /* This comment should go away */
      // to this:
      $display("INFO :[PIPE LINK %d]: ");

      /* also this comment should not get eaten
       because of use of slashes // and such like
       */
      /* 
       ugly hidded end comment // */
      $display("don't forget me agentina");
      // another hidden comment /*
      /**/
      
   end

endmodule // foo
// New File: <comment_strip.v>
`include "some_macros.v"

module z();
  
   $display("%t:", $time);
   a 			      = b;
   casfasdf 		      = d;
   g 			      = r;
   fgasdfasdfasdfasfdasfd    <= p;
   gh 			     := h;
   gf 			     <=g;
   ssdf 		      = 5;
   f 			      = zsfdsdf >= f;
  
endmodule
// New File: <debug.v>
module foo;
 task my_task;
 begin :body_my_task
 fork
 join
 case (a)
 endcase // case endcase
 if (a) begin
 end
 begin
 end
 fork : main_fork
 begin : body_main_fork
 fork : sub_fork
 begin
 // st1
 end
 begin
 // st2
 end
 join_any // first wins
 a  = b;
 disable fork; // kill others
 end // block: body_main_fork
 end // block: body_main_fork
 endtask // my_task
endmodule // foo
// New File: <disable.v>
module escape_a (/*AUTOARG*/
   // Outputs
   \o[10] , \o[2] ,
   // Inputs
   \i&e; 
   );
   output \o[10] ;
   output \o[2] ;
   input  \i&e; ;

   wire   \o[10] = \i&e; ;
   wire   \o[2] = \i&e; ;

endmodule
// New File: <escape_a.v>
module escape_top (/*AUTOARG*/
   // Outputs
   \oren10 , \o[2] ,
   // Inputs
   \i&e; 
   );

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		\i&e; ;			// To a of escape_a.v
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		\o[2] ;			// From a of escape_a.v
   output		\oren10 ;		// From a of escape_a.v
   // End of automatics

   /* escape_a AUTO_TEMPLATE(
	       .\(.*o.*10.*\)		(\\oren10 ),
    ); */

   escape_a a (/*AUTOINST*/
	       // Outputs
	       .\o[10] 			(\oren10 ),		 // Templated
	       .\o[2] 			(\o[2] ),
	       // Inputs
	       .\i&e; 			(\i&e; ));

endmodule
// New File: <escape_top.v>
        module ExampArg (/*AUTOARG*/);
          input i;
          output o;
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampArg.v>
        module ExampInject (i, o,/*AUTOARG*/
   // Inputs
   j
   );
          input i;
          input j;
          output o;
          always @ (/*AS*/i or j)
             o = i | j;
          InstModule instName
            (.foobar(baz),
             /*AUTOINST*/
             // Outputs
             .j                         (j));
        endmodule

        module InstModule (output j/*AUTOARG*/);
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInject.v>
        module ExampMain
          (input i,
           output o,
           inout io);
        endmodule

        module ExampInoutIn (/*AUTOARG*/
   // Inputs
   i, io, o
   );
           /*AUTOINOUTIN("ExampMain")*/
           // Beginning of automatic in/out/inouts (from specific module)
           input           i;
           input           io;
           input           o;
           // End of automatics
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInoutIn.v>
        interface ExampIf
          ( input logic clk );
           logic        req_val;
           logic [7:0]  req_dat;
           clocking mon_clkblk @(posedge clk);
              input     req_val;
              input     req_dat;
           endclocking
           modport mp(clocking mon_clkblk);
        endinterface

        module ExampMain
        ( input clk,
          /*AUTOINOUTMODPORT("ExampIf", "mp")*/
          // Beginning of automatic in/out/inouts (from modport)
          input           req_val,
          input [7:0]     req_dat
          // End of automatics
        );
        /*AUTOASSIGNMODPORT("ExampIf", "mp", "inst")*/
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInoutModport.v>
        module ExampMain ();
          parameter PARAM = 22;
        endmodule

        module ExampInoutParam ();
           /*AUTOINOUTPARAM("ExampMain")*/
           // Beginning of automatic parameters (from specific module)
           parameter       PARAM;
           // End of automatics
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInoutParam.v>
        module InstModule (inout io);
        endmodule

        module ExampInout (
           /*AUTOINOUT*/
           // Beginning of automatic inouts (from unused autoinst inouts)
           inout           io                     // To/From instName of InstModule.v
           // End of automatics
           );
           InstModule instName
             (/*AUTOINST*/
              // Inouts
              .io                       (io));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInout.v>
        module InstModule (input i);
        endmodule

        module ExampInput (
           /*AUTOINPUT*/
           // Beginning of automatic inputs (from unused autoinst inputs)
           input           i                       // To instName of InstModule.v
           // End of automatics
           );
           InstModule instName
             (/*AUTOINST*/
              // Inputs
              .i                        (i));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInput.v>
        module ExampInsertLisp;
           /*AUTOINSERTLISP(my-verilog-insert-hello "world")*/
           // Beginning of automatic insert lisp
initial $write("hello world");
           // End of automatics
        endmodule

        // For this example we declare the function in the
        // module's file itself.  Often you'd define it instead
        // in a site-start.el or init file.
        /*
         Local Variables:
         eval:
           (defun my-verilog-insert-hello (who)
             (insert (concat "initial $write(\"hello " who "\");\n")))
         End:
        */
// New File: <ExampInsertLisp.v>
        module InstModule (o,i);
           parameter PAR;
        endmodule

        module ExampInstParam (o,i);
           parameter PAR;
           InstModule #(/*AUTOINSTPARAM*/
                        // Parameters
                        .PAR            (PAR))
                instName (/*AUTOINST*/);
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInstParam.v>
        module InstModule (o,i);
           output [31:0] o;
           input i;
           wire [31:0] o = {32{i}};
        endmodule

        module ExampInst (o,i);
           output o;
           input i;
           InstModule instName
             (/*AUTOINST*/
              // Outputs
              .o                        (o[31:0]),
              // Inputs
              .i                        (i));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampInst.v>
module example (/*AUTOARG*/
   // Outputs
   lower_out, o,
   // Inputs
   lower_inb, lower_ina, i
   );
   input i;
   output o;
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		lower_ina;		// To inst of inst.v
   input		lower_inb;		// To inst of inst.v
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		lower_out;		// From inst of inst.v
   // End of automatics
   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg			o;
   // End of automatics
   inst inst (/*AUTOINST*/
	      // Outputs
	      .lower_out		(lower_out),
	      // Inputs
	      .lower_inb		(lower_inb),
	      .lower_ina		(lower_ina));
   always @ (/*AUTOSENSE*/i) begin
      o = i;
   end
endmodule


// New File: <example.v>
        module ExampOutputEvery (
           /*AUTOOUTPUTEVERY*/
           // Beginning of automatic outputs (every signal)
           output          o,
           output          tempa,
           output          tempb,
           // End of automatics
           input i
           );
           wire tempa = i;
           wire tempb = tempa;
           wire o = tempb;
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampOutputEvery.v>
        module InstModule (output o);
        endmodule

        module ExampOutput
           (/*AUTOOUTPUT*/
           // Beginning of automatic outputs (from unused autoinst outputs)
           output          o                       // From instName of InstModule.v
           // End of automatics
            );
           InstModule instName
             (/*AUTOINST*/
              // Outputs
              .o                        (o));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampOutput.v>
        module InstModule (o,i);
           parameter WIDTH;
           input [WIDTH-1:0] i;
           parameter type OUT_t;
           output OUT_t o;
        endmodule

        module vm_example1;
           /*AUTOOUTPUT*/
           // Beginning of automatic outputs (from unused autoinst outputs)
           output OUT_t    o;                      // From instName of InstModule.v
           // End of automatics

           InstModule
             #(.WIDTH(10),
               ,.OUT_t(upper_t))
            instName
             (/*AUTOINST*/
              // Outputs
              .o                        (o),
              // Inputs
              .i                        (i[WIDTH-1:0]));
        endmodule

        // Local Variables:
        // verilog-typedef-regexp: "_t$"
        // verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
        // End:
// New File: <ExampParamVal1.v>
        module InstModule (o,i);
           parameter WIDTH;
           input [WIDTH-1:0] i;
           parameter type OUT_t;
           output OUT_t o;
        endmodule

        module vm_example1;
           /*AUTOOUTPUT*/
           // Beginning of automatic outputs (from unused autoinst outputs)
           output upper_t  o;                      // From instName of InstModule.v
           // End of automatics

           InstModule
             #(.WIDTH(10),
               ,.OUT_t(upper_t))
            instName
             (/*AUTOINST*/
              // Outputs
              .o                        (o),
              // Inputs
              .i                        (i[9:0]));
        endmodule

        // Local Variables:
        // verilog-typedef-regexp: "_t$"
        // verilog-auto-inst-param-value: t
        // verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
        // End:
// New File: <ExampParamVal2.v>
        module InstModule (input i);
        endmodule

        module ExampRegInput ();
           /*AUTOREGINPUT*/
           // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
           reg             i;                      // To instName of InstModule.v
           // End of automatics
           InstModule instName
             (/*AUTOINST*/
              // Inputs
              .i                        (i));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampRegInput.v>
        module ExampReg (o,i);
           output o;
           input i;
           /*AUTOREG*/
           // Beginning of automatic regs (for this module's undeclared outputs)
           reg             o;
           // End of automatics
           always o = i;
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampReg.v>
        module ExampReset ();
           always @(posedge clk or negedge reset_l) begin
              if (!reset_l) begin
                  c <= 1;
                  /*AUTORESET*/
                  // Beginning of autoreset for uninitialized flops
                  a <= 1'h0;
                  b <= 1'h0;
                  // End of automatics
              end
              else begin
                  a <= in_a;
                  b <= in_b;
                  c <= in_c;
              end
           end
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampReset.v>
        module ExampMain
          (input i,
           output o,
           inout io);
        endmodule

        module ExampInOutIn (/*AUTOARG*/
   // Inputs
   i, io, o
   );
           /*AUTOINOUTIN("ExampMain")*/
           // Beginning of automatic in/out/inouts (from specific module)
           input           i;
           input           io;
           input           o;
           // End of automatics
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampShell.v>
        module ExampMain
          (input unused_input_a, input unused_input_b);
        endmodule
    
        module ExampStub2 (/*AUTOARG*/
   // Inputs
   unused_input_a, unused_input_b
   );
            /*AUTOINOUTPARAM("ExampMain")*/
            /*AUTOINOUTMODULE("ExampMain")*/
            // Beginning of automatic in/out/inouts (from specific module)
            input           unused_input_a;
            input           unused_input_b;
            // End of automatics
    
            /*AUTOTIEOFF*/
    
            // verilator lint_off UNUSED
            wire _unused_ok = &{1'b0,
                                /*AUTOUNUSED*/
                                // Beginning of automatic unused inputs
                                unused_input_a,
                                unused_input_b,
                                // End of automatics
                                1'b0};
            // verilator lint_on  UNUSED
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampStub2.v>
        module ExampMain
          #(parameter P)
          (input i, output o, inout io);
        endmodule
    
        module ExampStub (/*AUTOARG*/
   // Outputs
   o,
   // Inouts
   io,
   // Inputs
   i
   );
            /*AUTOINOUTPARAM("ExampMain")*/
            // Beginning of automatic parameters (from specific module)
            parameter       P;
            // End of automatics
            /*AUTOINOUTMODULE("ExampMain")*/
            // Beginning of automatic in/out/inouts (from specific module)
            output          o;
            inout           io;
            input           i;
            // End of automatics
    
            /*AUTOTIEOFF*/
            // Beginning of automatic tieoffs (for this module's unterminated outputs)
            wire            o                       = 1'h0;
            // End of automatics
    
            // verilator lint_off UNUSED
            wire _unused_ok = &{1'b0,
                                /*AUTOUNUSED*/
                                // Beginning of automatic unused inputs
                                i,
                                io,
                                // End of automatics
                                1'b0};
            // verilator lint_on  UNUSED
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampStub.v>
        `define XX_FOO
        `define M_BAR(x)
        `define M_BAZ
        ...
        `ifdef NEVER
          `undef M_BAZ  // Emacs will see this and not `undef M_BAZ
        `endif
        ...
        /*AUTOUNDEF*/
        // Beginning of automatic undefs
        `undef M_BAR
        `undef XX_FOO
        // End of automatics

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampUndef.v>
        module InstModule (o,i);
           output [31:0] o;
           input i;
           wire [31:0] o = {32{i}};
        endmodule

        module ExampWire (i);
           input i;
           /*AUTOWIRE*/
           // Beginning of automatic wires (for undeclared instantiated-module outputs)
           wire [31:0]     o;                      // From instName of InstModule.v
           // End of automatics
           InstModule instName
             (/*AUTOINST*/
              // Outputs
              .o                        (o[31:0]),
              // Inputs
              .i                        (i));
        endmodule

// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <ExampWire.v>

module flag_f_reeves
  (/*AUTOARG*/) ;

   /*AUTOOUTPUT*/

   /*AUTOINPUT*/

   flag_f_reeves_IBUF ibuf
     (/*AUTOINST*/);

endmodule
// Local Variables:
// verilog-library-flags: ("-f flag_f_reeves.vc")
// End:
// New File: <flag_f_reeves.v>

module flag_f_reeves
  (/*AUTOARG*/) ;

   /*AUTOOUTPUT*/

   /*AUTOINPUT*/

   flag_f_reeves_IBUF ibuf
     (/*AUTOINST*/);

endmodule
// Local Variables:
// verilog-library-flags: ("-F subdir/flag_frel_reeves.vc")
// End:
// New File: <flag_frel_reeves.v>
module lbm
  (/*AUTOARG*/
   // Outputs
   outgo,
   // Inputs
   income
   );

   input	[1:0]	income;
   output [1:0] 	outgo;
   reg [1:0] 		outgo;

   integer 		i;
   always @ (/*AUTOSENSE*/income) begin
      for (i=0; i<32; i=i+1) begin
	 outgo[i] = income[i];
      end
   end

   always @ (/*AUTOSENSE*/income) begin
      if (|income) begin
	 $display("[0%10t] %I-for.v: found %s \"quote\" ",
		  $time, ((|income)?"in":"out"));

      end
   end

endmodule
// New File: <for.v>
module x;
   always @(/*autosense*/d32S_CurCcbH or ramS_CcbBaseH or ramS_ccbsizeH)
     begin
	case (ramS_ccbsizeH)
	  2'b00 : ramS_CcbAddH = {2'b00, d32S_CurCcbH };
	  2'b01 : ramS_CcbAddH = {1'b0, d32S_CurCcbH, 1'b0};
	  2'b10 : ramS_CcbAddH = {d32S_CurCcbH, 2'b00};
	  2'b11 : ramS_CcbAddH = {d32S_CurCcbH, 2'b00}; // unused
	endcase
	ramS_CcbAddresH = {10'h000, ramS_CcbAddH} + ramS_CcbBaseH;
     end
endmodule
// New File: <gorfajn.v>
module foo (/*AUTOARG*/
   // Outputs
   d,
   // Inputs
   b, a
   );
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		a;			// To foo2 of foo2.v
   input		b;			// To foo2 of foo2.v
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output		d;			// From foo2 of foo2.v
   // End of automatics
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics
   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   // End of automatics
   foo2 foo2 (/*AUTOINST*/
	      // Outputs
	      .d			(d),
	      // Inputs
	      .a			(a),
	      .b			(b));

endmodule


module foo2 (/*AUTOARG*/
   // Outputs
   d,
   // Inputs
   a, b
   );
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   // End of automatics
   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics
   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   // End of automatics
   input a;
   input b;
   output d;

   //{ Behavioral verilog code here}
endmodule

// New File: <grisamore_twoinone.v>
// See Line 54

module hangcase (/*AUTOARG*/);

   //
   assign w_rdat_ena       = ({16{foo[ 0]}} & bar ) |
			     ({16{foo[ 1]}} & bar ) |
			     ({16{foo[ 2]}} & bar ) |
			     ({16{foo[ 3]}} & bar ) |
			     ({16{foo[ 4]}} & bar ) |
			     ({16{foo[ 5]}} & bar ) |
			     ({16{foo[ 6]}} & bar ) |
			     ({16{foo[ 7]}} & bar ) |
			     ({16{foo[ 8]}} & bar ) |
			     ({16{foo[ 9]}} & bar ) |
			     ({16{foo[10]}} & bar ) |
			     ({16{foo[11]}} & bar ) |
			     ({16{foo[12]}} & bar ) |
			     ({16{foo[13]}} & bar ) |
			     ({16{foo[14]}} & bar ) |
			     ({16{foo[15]}} & bar ) ;

   //
   assign w_rdat_mrk       = ({16{foo[ 0]}} & bar & baz ) |
			     ({16{foo[ 1]}} & bar & baz ) |
			     ({16{foo[ 2]}} & bar & baz ) |
			     ({16{foo[ 3]}} & bar & baz ) |
			     ({16{foo[ 4]}} & bar & baz ) |
			     ({16{foo[ 5]}} & bar & baz ) |
			     ({16{foo[ 6]}} & bar & baz ) |
			     ({16{foo[ 7]}} & bar & baz ) |
			     ({16{foo[ 8]}} & bar & baz ) |
			     ({16{foo[ 9]}} & bar & baz ) |
			     ({16{foo[10]}} & bar & baz ) |
			     ({16{foo[11]}} & bar & baz ) |
			     ({16{foo[12]}} & bar & baz ) |
			     ({16{foo[13]}} & bar & baz ) |
			     ({16{foo[14]}} & bar & baz ) |
			     ({16{foo[15]}} & bar & baz ) ;

   //
   assign w_wdat_ena_set  = ({16{ena_set}} & col_dec    );
   assign w_wdat_ena_clr  = ({16{ena_clr}} & col_dec    );
   assign w_wdat_mrk_set  = ({16{mrk_set}} & w_rdat_ena );
   assign w_wdat_mrk_clr  = ({16{mrk_clr}} & col_dec    );
   assign w_wdat_ena      = (w_rdat_ena & ~w_wdat_ena_clr) | w_wdat_ena_set;
   assign w_wdat_mrk      = (w_rdat_mrk & ~w_wdat_mrk_clr) | w_wdat_mrk_set;

   //
   assign w_dat15_ena     = foo[15] ? w_wdat_ena : bar;

   //
   assign w_dat15_mrk     = foo[15] ? w_wdat_mrk : baz;

   //^^^^ FIX NEWLINE ABOVE HERE
   //
   assign w_timeout_mrk     = row_check      ?  w_wdat_mrk        : r_timeout_mrk;

endmodule
// New File: <hangcase.v>
`ifndef TEST
 `define TEST "autolisp_include_inc.vh"
`endif

`include `TEST

///////////////////////////////////////////////////////////////////////////////
// Local Variables:
// verilog-library-directories: (".")
// eval:(verilog-read-defines)
// eval:(verilog-read-includes)
// End:
// New File: <include_defined_bug1324.v>
module foo(reg_input_signal_name);
   input a;
   input    reg_input_signal_name;
   
endmodule // foo

// New File: <indent_1.v>
module foo(reg_input_signal_name,a,b,c,d,e);
     // foo bar
   output 	     c;//this is a comment

   
  reg foo;
                                                  //a

             /*  jj 
	         KK */
reg foo; 
   output        reg  /*   */  signed 	     d;
output reg signed e; /* so is this */

   reg [31:0] 	     blather;
   
endmodule // foo

// New File: <indent_2.v>
module junk (/*AUTOARG*/) ;
   input a_junk;
   input wire signed [15:0] 			  b_junk;
   output c_junk;
   output [15:0][31:0][1024:(`REALLY_BIG_NAME-1)] d_junk;
endmodule // junk

// New File: <indent_3.v>
module foo;
   initial
     begin
       s1;
     end

   always @(a)
     begin
	s1;
     end // always @ (a)
   always
     begin
	s1;
     end // always begin
   always_ff
     begin
     end // always_ff begin
   task
     t;
   endtask // t

endmodule // foo
// New File: <indent_4.v>
module foo ();
   // Before the always block,
   // indents to here: (which I like)
   //          V
   wire [10:0] mux_output0 =
               select0[0] ? mux_input0 :
               select0[1] ? mux_input1 :
               select0[2] ? mux_input2 :
               select0[3] ? mux_input3 :
               select0[4] ? mux_input4 :
               select0[5] ? mux_input5 :
               select0[6] ? mux_input6 :
               mux_input7;
   
   reg 	       foobar;
   always @(posedge clk)
     if (reset_n) begin
        foobar <= 1'b0;
     end
   
   // After the always block,
   // indents to here:
   //                       V
   wire [10:0] mux_output1 =
               select1[0] ? mux_input8 :
               select1[1] ? mux_input9 :
               select1[2] ? mux_input10 :
               select1[3] ? mux_input11 :
               select1[4] ? mux_input12 :
               select1[5] ? mux_input13 :
               select1[6] ? mux_input14 :
               mux_input15;
endmodule

// New File: <indent_always_decl.v>

module mymodule();
    parameter real fc=10e6;
  parameter real bw=25e3;
analog begin
// contents of module here
   end
     endmodule
// New File: <indent_analog.v>
module myassert(input clk,
                input        reset,
                input [15:0] data);
   
   property myproperty;
      @(posedge clk)
        $rose(reset) |-> data  == 16'h0;
   endproperty
   
   //Assert, cover, and assume property statements
   //support begin/end keywords.  The else begin/end
   //clause below is getting indented improperly.
   myassert0: assert property(myproperty) begin
      $display("myassert0 was successful");
      a;
      b;
      c;
      d;
   end // myassert0: assert property (myproperty)
   else begin
      $fatal("myassert0 was unsuccessful");
   end // else: !assert property(myproperty)
   if (a) begin
      b;
      c;
   end // if (a)
   else begin
      o;
   end // else: !if(a)
   assert (a) begin
      o;
   end // assert (a)
   else begin
      o;
   end // else: !assert (a)
   
   assert (statement) begin
      $display("assertion passed"); //this code is correctly indented
   end // assert (statement)
   else begin // this whole section should be moved to the left
      $error("assertion failed");
   end // else: !assert (statement)
   
   //Also, any statements following the assert,
   //cover, and assume property statements get
   // indented too far to the right.
   always @(posedge clk) begin
      a;
   end // always @ (posedge clk)
endmodule
// New File: <indent_assert_else.v>
module myassert(input clk, 
		input reset, 
		input [15:0] data);
   
   property myproperty;
      @(posedge clk)
	$rose(reset) |-> data == 16'h0;
   endproperty
   
   //Assert, cover, and assume property statements
   //support begin/end keywords.  The else begin/end
   //clause below is getting indented improperly.
   myassert0: assert property(myproperty) begin
      $display("myassert0 was successful");
   end
else begin
   $fatal("myassert0 was unsuccessful");
end

   //Also, any statements following the assert,
   //cover, and assume property statements get
   // indented too far to the right.
   always @(posedge clk) begin
   end
endmodule

// https://github.com/veripool/verilog-mode/issues/1836
module tb1;
 a: restrict property (1);
   b: assume property (1);
     c: assume property (1);
       endmodule

// https://github.com/veripool/verilog-mode/issues/1837
module tb2;
         a: cover sequence (1);
     b: cover property (1);
  c: cover property (1);
endmodule
// New File: <indent_assert_property.v>
module assert_test;
   reg [31:0] whatever2;
   initial begin
      a = b;
      assert(std::randomize(whatever2)  with { whatever2   inside {[10:100]};});
   end
endmodule // assert_test
// New File: <indent_assert.v>
// Issue 941 : The following operators should not be broken by auto-indents
module m;
initial begin
a = b;
a <= b;
a <<= b;
a <<<= b;
a >= b;
a >>= b;
a >>>= b;
a == b;
a != b;
a === b;
a !== b;
a ==? b;
a !=? b;
a <-> b;
a -> b;
a ->> b;
a |-> b;
a |=> b;
a #-# b;
a #=# b;
a := b;
a :/ b;
end

// bug1169
assign a = t ?
m :
n;
assign a = z ?
m :
n;

endmodule
// New File: <indent_assignment.v>
module example(out1, out2, out3);
   (* LOC = "D14" *)
   output out1;
   /* foobar */ (* LOC = "C15" *) /* jar */ output out2;
   (* LOC = "C16" *)
   output out3;
   out1 = 1'b1;
   out2 = 1'b1;
   out3 = 1'b1;
endmodule
// New File: <indent_attributes.v>
// bug 825
module x;

always @*
begin
end

initial
begin
end

final
begin
end

initial forever
  begin
end

foreach(1)
begin
  end

do
 begin
 end while (i);

initial @a.b
  begin
end

always @E
  begin
 end

forever @E
 begin
  end

endmodule

// Local Variables:
// verilog-indent-begin-after-if: nil
// End:
// New File: <indent_begin_clapp.v>
// bug437:  Indentation of continued assignment incorrect if first line ends with ']'
module foo
(input [63:0]  data_in,
 input         ctl_in,

output [63:0] data_out,
output        ctl_out);

assign data_out  = data_in[1] ? data_in[63:0]
:            64'h0;

endmodule
// New File: <indent_bracket.v>
module testcaseindent();
   case (a)
     1: begin
	asdf;
     end // case: 1
     
     2: begin
	asdfasdf;
     end // case: 2
     
     3: begin
	asdfasd;
     end // case: 3
     
   endcase // case (a)
   
   
   unique case (a)
     1: begin
	asdf;
     end // case: 1
     
     2: begin
	case (d)
	  2: a;
	  3: c;
	  4: begin
	     priority casex (f)
	       4: g;
	       5: h;
	     endcase // priority casex
	  end
	endcase // case (d)
	asdfasdf;
     end // case: 2
     
     3: begin
	asdfasd;
     end // case: 3
	  
	endcase // case (a)
   unique case (a)
     1: asdf;
     2: asdfasdf;
     3: asdfasd;
   endcase // case (a)
   
endmodule // test_case_indent
// New File: <indent_case.v>
class foo();
 int my_field;
endclass // foo

class temp;
 extern function test();
 extern function test2();
 function foo();
 foo = 1;
 endfunction // foo
 extern function test3();
 reg [31:0] b;
endclass // temp

class short extends temp;
 logic a;
endclass

`define vmm_channel(A) A+A


module foo;
 reg a;
 reg [1:0] b;

 initial begin
 b = `vmm_channel(a);
 end // initial begin
endmodule // foo

 
class A;
   extern function int e1();
   extern function int e2(int src,int dst);
   extern static function int f1();
   extern static function int f2(int src,int dst);
   extern static function int f3(int src,int dst);
   extern static function chandle f10(int src);
   extern static function automatic int f11(int mcid);
   extern function automatic int f13(int mcid);
   static function int s1();
      int i = 0;
   endfunction
   static function int s2();
      int i = 0;
   endfunction
   function int f1();
      int i = 0;
   endfunction
   function int f2();
      int i = 0;
   endfunction
endclass
// New File: <indent_class.v>
class mipsbfm_trans extends vmm_data;
static vmm_log log = new ("mipsbfm_trans", "class") ;
logic [31:0] addr, data, mask, op;
function new();
super.new(this.log);
endfunction: new
endclass // mipsbfm_trans

interface mipsbfm_if(input clk);
logic [31:0] data;
logic [31:0] addr;
logic [31:0] mask;
logic [31:0] op;
logic 	valid;

clocking cb @(posedge clk);
output 	data;
output 	addr;
output 	mask;
output 	op;
output 	valid;
endclocking // cb

endinterface // mipsbfm_if


`vmm_channel(mipsbfm_trans);

//--------------------------------------------------------------
// MIPS BFM Master Xactor Class
//--------------------------------------------------------------

class mipsbfm_master extends vmm_xactor;
// Transaction channels
mipsbfm_trans_channel  in_chan ;

endclass // mipsbfm_master

// New File: <indent_clockingblock.v>
module t;
   default clocking @(posedge clk);
      begin
	 a  = 8;
      end
   endclocking
   default clocking clocking_identifier;
      property foo (a)
	a   = b;
      endproperty
      cover property (prop) $display("**COVERAGE**");
      assert property (foo) a;
      assume property (bar) b;
      b1: assume property (bar) b;
	 B2: assert property (foo) a;
	    B2: cover property (foo) a;
	       assume property (bar) b;	       
	       a;
endmodule
// New File: <indent_clocking.v>
// Bug 1717
module test
  (input wire test5 // bla
   /*AUTOARG*/);

`include "bla.vh"

   reg        test4;

   wire       test3;

   //  // FIXXME
              wire       test1 = test2; // wrong indent
   wire                  test = test;

endmodule
// New File: <indent_comments_bug1717.v>
module dummy (
	      input wire xx,
	      output wire yy, // comment with paren ) adsfasdf
	      output wire zz); // oops - matched paren in comment!!
endmodule // dummy
// New File: <indent_comments.v>

connectmodule Wreal2L(input wreal w, output wire l);
logic lval;

  parameter real vsup = 1.8;
       parameter real vth_hi = 0.7*vsup;
  parameter real vth_lo = 0.3*vsup;
  always begin
if( w >= vth_hi) lval = 1'b1;
    else if(w <= vth_lo) lval = 1'b0;
    else if(w === `wrealZState) lval = 1'bz;
    else lval = 1'bx;
    @(w);
end

assign l = lval;
   endconnectmodule

// New File: <indent_connectmodule.v>

constraint Cepon_gate_random::grant_relations_c {
						 foreach (grant_relation_m[index]) {
										    if ((index == 0) && (last_start_time_m < time_stamp_m+`MINIMUM_TIME_TO_GATE)) grant_relation_m[index] == EPON_GATE_IN_ORDER;
										    
										    else {
   
   grant_relation_m[index] dist {
				 EPON_GATE_IN_ORDER :/55,
				 EPON_GATE_1ST_CONTAINS_2ND :/5,
				 EPON_GATE_2ND_CONTAINS_1ST :/5,
				 EPON_GATE_1ST_START_2ND_START :/5,
				 EPON_GATE_2ND_START_1ST_START :/5,
				 EPON_GATE_NO_GAP_1ST :/5,
				 EPON_GATE_NO_GAP_2ND :/5,
				 EPON_GATE_1CC_GAP :/5,
				 EPON_GATE_1CC_INTERLEAVE :/5,
				 EQUAL :/5
                        };

}
										    
										    }
						 
						 }
// New File: <indent_constraint2.v>
//bug433

class data;
   rand integer data1;
   rand integer data2,data3;
   rand reg [31:0] foo;
    
   constraint basic_c {
		       //empty constraint
		       }
      
     constraint complex_c {
			   data1 <= 100;
			   foo inside {[40:999]};
			   if(foo < 87)
			   data2 == 10;
			   if(data2 == 76) {
	data3 == 8;
     }
			   }
	
       constraint implication_c {
	  data1 == 10 -> data3 >= -1;
       }
   
   function new();
      data1 = 0;
      data2 = 78;
   endfunction // new
endclass // data
// New File: <indent_constraint3.v>
// my simple if else example, indented by verilog-mode
if (x == 1)
 begin
 test1 <= 1;
 test2 <= 2;
 end
else
 begin
     test1 <= 2;
 test2 <= 1;
 end

// code from IEEE spec, pg. 164
class MyBus extends Bus;
 rand AddrType atype;
 constraint addr_range
 {
 (atype == low ) -> addr inside { [0 : 15] };
 (atype == mid ) -> addr inside { [16 : 127]};
 (atype == high) -> addr inside {[128 : 255]};
 }
 //
endclass // MyBus

// same example, with verilog mode indenting, Cexp indent = 3
class MyBus extends Bus;
 rand AddrType atype;
 constraint addr_range
 {
 (atype == low ) -> addr inside { [0 : 15] };
 (atype == mid ) -> addr inside { [16 : 127]};
 (atype == high) -> addr inside {[128 : 255]};
 }
 //
endclass // MyBus

// same example, with verilog mode indenting, Cexp indent = 0
class MyBus extends Bus;
 rand AddrType atype;
 constraint addr_range
 {
 (atype == low ) -> addr inside { [0 : 15] };
 (atype == mid ) -> addr inside { [16 : 127]};
 (atype == high) -> addr inside {[128 : 255]};
 }
endclass // MyBus

// covergroup example from IEEE pg. 317
covergroup cg @(posedge clk );
 a : coverpoint v_a {
 bins a1 = { [0:63] };
 bins a2 = { [64:127] };
 bins a3 = { [128:191] };
 bins a4 = { [192:255] };
 }
 b : coverpoint v_b {
 bins b1 = {0};
 bins b2 = { [1:84] };
 bins b3 = { [85:169] };
 bins b4 = { [170:255] };
 }
 //
 c : cross a, b
 {
 bins c1 = ! binsof(a) intersect {[100:200]}; // 4 cross products
 bins c2 = binsof(a.a2) || binsof(b.b2); // 7 cross products
 bins c3 = binsof(a.a1) && binsof(b.b4); // 1 cross product
 }
endgroup

// here is the same code with verilog-mode indenting
// covergroup example from IEEE pg. 317
covergroup cg @(posedge clk );
 a : coverpoint v_a
 {
 bins a1 = { [0:63] };
 bins a2 = { [64:127] };
 bins a3 = { [128:191] };
 bins a4 = { [192:255] };
 }
 // foo
 b : coverpoint v_b
 {
 bins b1 = {0};
 bins b2 = { [1:84] };
 bins b3 = { [85:169] };
 bins b4 = { [170:255] };
 }
 c : cross a, b
 {
 bins c1 = ! binsof(a) intersect {[100:200]}; // 4 cross products
 bins c2 = binsof(a.a2) || binsof(b.b2); // 7 cross products
 bins c3 = binsof(a.a1) && binsof(b.b4); // 1 cross product
 }
endgroup

module fool;

 always @(posedge clk) begin
 if(!M_select)
 xferCount < = 8'd0;
 else
 case (condition[1 :0])
 2'b00 : xferCount <= xferCount;
 2'b01 : xferCount <= xferCount - 8'd1;
 2'b10 : xferCount <= xferCount + 8'd1;
 2'b11 : xferCount <= xferCount;
 endcase // case (condition[1:0])
 end
 // But not this :
 always @(posedge clk) begin
 if(!M_select)
 xferCount < = 8'd0;
 else
 case ({M_seqAddr,OPB_xferAck})
 2'b00 : xferCount <= xferCount;
 2'b01 : xferCount <= xferCount - 8'd1;
 2'b10 : xferCount <= xferCount + 8'd1;
 2'b11 : xferCount <= xferCount;
 endcase // case ({M_seqAddr,OPB_xferAck})
 end // always @ (posedge clk)
 
endmodule // fool

module foo;
 initial begin
 k = 10;
 std::randomize(delay) with { (delay>=1000 && delay<=3000); };
 j = 9;
 end
endmodule // foo


// Issue 324 - constraint indentation is not correct
// This checks for indentation around { and } inside constraint contents
class myclass;
constraint c {
foreach(items[i]) {
if(write) {
items[i].op_code == WRITE;
} else if(read) {
items[i].op_code == READ;
}
}
}
endclass // myclass
// New File: <indent_constraint.v>
module m;
   bit[0:0] a, b, c;
   covergroup g;
            cp_ab: coverpoint {a,b} {
				     bins one = {1};
				     bins two = {2};
				                                    }

	              cp_ab_if_c: coverpoint {a,b} iff c {
							  bins one = {1};
							  bins two = {2};
							          }

		              cp_ab_if_c_slice: coverpoint {a,b} iff c[0] {
									   bins one = {1};
									   bins two = {2};
									                                                        }

				          cp_a_if_bc: coverpoint {a,b} iff {b,c} {
										  bins one = {1};
										  bins two = {2};
										                                                    }

					                cp_a_slice : coverpoint a[0] {
										      bins one = {1};
										      bins two = {2};
										                                                }

							                cp_a_slice_if_b : coverpoint a[0] iff b {
														 bins one = {1};
														 bins two = {2};
														               }

									              cp_a_if_b_slice : coverpoint a iff b[0] {
															       bins one = {1};
															       bins two = {2};
															                                                              }

											                cp_a_slice_if_b_slice : coverpoint a[0] iff b[0] {
																			  bins one = {1};
																			  bins two = {2};
																			                                                                    }
													  endgroup
endmodule
// New File: <indent_covergroup_swan.v>
module coverage;
   enum { red, green, blue } color;
   covergroup foo @(posedge clk);
      c : coverpoint color;
      c : coverpoint color;
      c : coverpoint color;
      c : coverpoint color;      
   endgroup // foo
   
   foo 	  = bar;
   
   sequence bar
     b 	  = c;
   endsequence // bar
   j 	  = taskt;
   function foo;
      begin
	 foo 	= 1;
      end
   endfunction // foo
   
   randsequence bar
     b 		= c;
   endsequence // bar
   
   case (foo)
     1: a;
     2:b;
   endcase // case (foo)
   
   casex (foo)
     1: a;
     2:b;
   endcase // case (foo)
   
   casez (foo)
     1: a;
     2:b;
   endcase // case (foo)
   
   randcase (foo)
     1: a;
     2:b;
   endcase // case (foo)
   
endmodule // coverage
// New File: <indent_covergroup.v>

// here is the same code with verilog-mode indenting
// covergroup example from IEEE pg. 317
covergroup cg @(posedge clk);
a: coverpoint v_a {
bins a1 = { [0:63] };
bins a2 = { [64:127] };
bins a3 = { [128:191] };
bins a4 = { [192:255] };
}
b: coverpoint v_b iff ( g )
{
bins b1 = {0};
bins b2 = { [1:84] };
bins b3 = { [85:169] };
bins b4 = { [170:255] };
}
c: coverpoint v_c iff ( !g ) {
bins c1 = {0};
bins c2 = { [1:84] };
bins c3 = { [85:169] };
bins c4 = { [170:255] };
}
d: cross a , b, c {
bins c1 = ! binsof(a) intersect {[100:200]}; // 4 cross products
bins c2 = binsof(a.a2) || binsof(b.b2); // 7 cross products
bins c3 = binsof(a.a1) && binsof(b.b4); // 1 cross product
}
endgroup
// New File: <indent_coverpoint.v>
module soft_rst
  (
   // System clock and reset
   input  clk,
   input  rst_n,
   
   // Interface to software land
   input  req_soft_rst, // Soft reset request
   output soft_rst_dne, // Soft reset done
   
   // Interface to other modules
   output dma_halt, // Reset pending, halt activity
   input  tx_quiet, // TX side is dormant
   input  rx_quiet, // RX side is dormant
   input  c,
   output soft_rst, // Soft (sync) reset to VC3 side
   output hs_async_rst_n  // Async reset to host side
   );

   reg [1:0]    state;

   reg [1:0] IDLE = 2'h0,
     HALT  = 2'h1,
     RST   = 2'h2,
     DONE  = 2'h3;
   
endmodule // soft_rst
// New File: <indent_decl-1.v>
module foo;
   input a;
   input b;
   input [1:0] cfg_dev_i; // Config Device: 0b00 = Pangu, 0b01 = Switch
   output switch_idsel_o; // Switch PCI IDSEL
   output pangu_idsel_o;                    // Pangu PCI IDSEL
   output wire g;
   inout wire  h;
endmodule // foo
// New File: <indent_decl.v>
module foo;
`ifdef LABEL_A
   CHIP CPU (
             .clkin(clkin),
 `ifdef LABEL_B
             .bclko(bclko),
 `endif
             .cmode(cmode),
             );
   input sysclk;
 `ifdef LABEL_B
   input bclko;
 `endif
   input cmode;
`endif
   
   // instead of:
   
`ifdef LABEL_A
   CHIP CPU (
             .clkin(clkin),
 `ifdef LABEL_B
             .bclko(bclko),
 `endif
             .cmode(cmode),
             );
   input sysclk;
 `ifdef LABEL_B
   input bclko;
 `endif
   input cmode;
`endif //  `ifdef LABEL_A
   reg 	 a,b;
`ifdef A
   always @(a) begin
      b = a; // asfSDfsdfsasa
      b = a; // asfSDfsdfsasa
      b = a; // asfSDfsdfsasa      //
      b = a; // asfSDfsdfsasa      //       
      b = a; // asfSDfsdfsasa      //
      b = a; // asfSDfsdfsasa      //       
      b = a; // asfSDfsdfsasa      //       
      b = a; // asfSDfsdfsasa      //       
      b = a; // asfSDfsdfsasa      //       
   end
`elsif B
   always @(b) begin
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa      //       
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa      //       
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa      //       
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa
      a = b; // asfSDfsdfsasa      //       
   end
`else // !`elsif B
   always @(a or b) begin
      a <= b;
      b <= a;
   end
`endif // !`elsif B
   
   
endmodule // foo
// New File: <indent_directives.v>
module test;
always_ff @(posedge clk or negedge rst_n)
    if (~rst_n)
begin
 a <= {(5){1'b0}};
     a <= 1;
   end

always_ff @(posedge clk or negedge rst_n)
if (~rst_n)
     begin
   a <= {5{1'b0}};
 a <= 1;
      end

always_ff @(posedge clk or negedge rst_n)
  if (~rst_n)
    begin
      a <= {{1'b0,1'b0}};
a <= 1;
 end

always_ff @(posedge clk or negedge rst_n)
  if (~rst_n)
   begin
       a <= {b, {1'b0,1'b0}};
 a <= 1;
        end

always_ff @(posedge clk or negedge rst_n)
    if (~rst_n)
     begin
  a <= {b[1:0], {1'b0,1'b0}};
   a <= 1;
       end
  endmodule
// New File: <indent_double_curly.v>
   import "DPI-C" function string fna (input string str1);
export "DPI" c_identifier = task task_identifier;
  import "DPI" context function string fnb (input string str1);

module testbench;

    import "DPI" function string fn1 (input string str1);
   import "DPI-C" function void dpiWriteArray (input bit[7:0] data[]);
      import "DPI-C" pure function void dpiReadArray (output bit[7:0] data[]);
 import "DPI-C" function void dpiAesSetKey ( int key_high_u int key_high_l,
                                               int key_low_u, int key_low_l );
  import "DPI-C" function void dpiAesSetIV (int iv_high_u, int iv_high_l,
                                             int iv_low_u, int iv_low_l);
    import "DPI-C" function void dpiAesSetSkip (int skip);
  import "DPI-C" function void dpiAesCBCEncrypt ();

   logic 					 a;
endmodule // testbench

/*
package ref_model;
   import "DPI-C" xx_write_bmp_file =
     function void write_bmp_file(input string filename);
        
   import "DPI-C" xx_demosaic =
     function void demosaic(regs regs,
                            inout pix_buf imgR, imgG, imgB);
endpackage 
*/
// New File: <indent_dpi.v>
module indent_enum;

enum int unsigned {
STATE_0 = 0,
STATE_2 = 2
} state;

enum int unsigned {
STATE_0 = 0,
STATE_1,
STATE_2
} state, next;

endmodule
// New File: <indent_enum.v>
`ifndef FOO
 `define FOO
module foo;
   reg [31:0] payload [255:0];
   reg [7:0]  index;
   
   always @ (b) begin
      foreach (1) begin
	 @(router.cb);
      end // foreach (1)
   end // always @ (b)
endmodule // foo
`else // !`ifndef FOO
`endif // !`ifndef FOO
// New File: <indent_foreach.v>
module fork_join_any;
   initial begin
      fork
         begin
            fork
               begin
               end
            join_any
            a  = b;
            disable fork;
	    blah;
	    wait fork;
 	    blah;
         end
      join_any
      foo  = bar;
   end // initial fork
endmodule // fork_join_any

class x;
   task y;
      a = b;
      wait fork;
      $display("I'm indented too far");
   endtask // y
endclass // x

// New File: <indent_fork_join_any.v>

module x;

   foo;
endmodule

bar;
module z;
endmodule
// New File: <indent_formfeed.v>
module t;
   
endmodule
class C;
   function int f();
      f  = 17;
   endfunction
   extern function int g();
   virtual function int f();
      a;
   endfunction // int
   // pure virtual functions have no endfunction.
   C a;
   initial begin
      $display("hello world");
      $display("a of f is %d, g is %d", a.f(),a.g());
   end
   function int C::g();
      g  = 18;
   endfunction // g
   // pure virtual functions have no endfunction.
endclass // C

class pure_virt_func_class;
   pure virtual function string pure_virt_func();
   pure virtual function string pure_virt_func();
   pure virtual function string pure_virt_func();
   extern pure virtual task t();
   pure virtual task t();
   virtual task t();
      /* body */
   endtask // t
   virtual function f();
      /* body */
   endfunction // f
endclass // pure_virt_func_class

class base_test extends uvm_test;
   `uvm_component_utils(base_test)
   typedef virtual my_if my_vif_t;
   // A function definition starting with the virtual keyword should not be
   // detected as a declaration. This issue is seen when an attempt to indent
   // each declaration is done (when the verilog-auto-lineup variable is set
   // to 'declarations).
   //   In other words, the "function" in "virtual function" below must not be
   // aligned with "my_if" in the "typedef virtual my_if.." line above.
   virtual function void start_of_simulation_phase(uvm_phase phase);
      super.start_of_simulation_phase(phase);
   endfunction : start_of_simulation_phase
endclass : base_test
// New File: <indent_function.v>
// Issue #1257

module t1
 (
 );

genvar pipe;
logic [1:0] v;
logic x;

//generate
for (pipe=0; pipe<2; pipe++) begin : v_bl
     always_comb begin
         assign v[pipe] = '0;
     end
     assign x = '0;  // will be incorrectly indented to column 0
end
//endgenerate

endmodule




module indent;

    // CASE 1 indented wrong.  See end of module
    for (genvar aa = 0; aa < FF; aa++) begin : gen_hh
       for (genvar bb = 0; bb < FF; bb++) begin : gen_ii
          if (`asdf [aa]) begin : gen_jj

             always_ff @ (negedge cc [aa][bb]) begin
                if (dd [aa][bb])) begin
                   for (uint_t d_idx = 0; d_idx < 16; d_idx++)
                     ee [aa][bb].push_front (tx_dfe_out [aa][bb]  [15 - d_idx]);
                end
             end

    always_ff @ (posedge hs_clk) begin
       ee_size [aa][bb] = ee [aa][bb].size();
       if (ee_size [aa][bb] > 0)
         gg [aa][bb] <= ee [aa][bb].pop_front;
    end

end : gen_jj
       end : gen_ii
    end : gen_hh



    // this indents correctly without generate/endgenerate
    for (genvar aa = 0; aa < FF; aa++) begin : gen_hh
       for (genvar bb = 0; bb < FF; bb++) begin : gen_ii
          if (`asdf [aa]) begin : gen_jj
             assign a[aa][bb] = aa + bb;
             assign b[aa][bb] = aa + bb;
          end : gen_jj
       end : gen_ii
    end : gen_hh


    // this works now with gen/endgen
    generate
       for (genvar aa = 0; aa < FF; aa++) begin : gen_hh
          for (genvar bb = 0; bb < FF; bb++) begin : gen_ii
             always @ (negedge cc)
               a = 5;
             always @ (negedge dd)
               w = 2;
          end : gen_ii
       end : gen_hh
    endgenerate


    // CASE 1 again. No change but works - apparently since verilog-mode hit a generate statement above this line
    for (genvar aa = 0; aa < FF; aa++) begin : gen_hh
       for (genvar bb = 0; bb < FF; bb++) begin : gen_ii
          if (`asdf [aa]) begin : gen_jj

             always_ff @ (negedge cc [aa][bb]) begin
                if (dd [aa][bb])) begin
                   for (uint_t d_idx = 0; d_idx < 16; d_idx++)
                     ee [aa][bb].push_front (tx_dfe_out [aa][bb]  [15 - d_idx]);
                end
             end

             always_ff @ (posedge hs_clk) begin
                ee_size [aa][bb] = ee [aa][bb].size();
                if (ee_size [aa][bb] > 0)
                  gg [aa][bb] <= ee [aa][bb].pop_front;
             end

          end : gen_jj
       end : gen_ii
    end : gen_hh

endmodule : indent
// New File: <indent_generate_bug1257.v>
module indent_gen_case #(parameter P = 0)
(input d, output reg q);

generate
case (P)
0: always @(*)
q = d;

1: begin
always @(*)
q = d;
end

2: always @(*) begin
q = d;
end

3: begin
always @(*) begin
q = d;
end
end
endcase
endgenerate

endmodule




// Without generate keyword
module indent_gen_case #(parameter P = 0)
(input d, output reg q);


case (P)
0: always @(*)
q = d;

1: begin
always @(*)
q = d;
end

2: always @(*) begin
q = d;
end

3: begin
always @(*) begin
q = d;
end
end
endcase


endmodule
// New File: <indent_generate_case.v>
module indent_gen_for #(parameter P = 1)
(input d, output reg q);

genvar i;
generate
for (i = 0; i < P; i += 1) begin
always @(*) begin
q = d;
end
end

for (i = 0; i < P; i += 1) begin
always @(*)
q = d;
end
endgenerate

endmodule




// Without generate keyword
module indent_gen_for #(parameter P = 1)
(input d, output reg q);

genvar i;

for (i = 0; i < P; i += 1) begin
always @(*) begin
q = d;
end
end

for (i = 0; i < P; i += 1) begin
always @(*)
q = d;
end


endmodule
// New File: <indent_generate_for.v>
module indent_gen_if #(parameter P = 0)
(input d, output reg q);

generate
if (P == 0) begin
always @(*)
q = d;
end

if (P == 1) begin
always @(*) begin
q = d;
end
end

if (P == 1) begin
always @(*)
q = d;
end
else begin
always @(*)
q = d + 1'b1;
end
endgenerate

endmodule



// Without generate keyword
module indent_gen_if #(parameter P = 0)
(input d, output reg q);


if (P == 0) begin
always @(*)
q = d;
end

if (P == 1) begin
always @(*) begin
q = d;
end
end

if (P == 1) begin
always @(*)
q = d;
end
else begin
always @(*)
q = d + 1'b1;
end


endmodule
// New File: <indent_generate_if.v>
module test();

  reg [3:0] x;

  genvar    i;
  generate
  for(i=0; i<4; i=i+1) begin:a
  always @(*) begin
  x[i] = 1;
  end
  wire y = 0;
  end
  endgenerate
endmodule // test


module test();

  reg [3:0] x;

  genvar    i;

  for(i=0; i<4; i=i+1) begin:a
  always @(*) begin
  x[i] = 1;
  end
  wire y = 0;
  end

endmodule // test

// New File: <indent_generate.v>
// bug1163

module indent;

   generate for (genvar i=0; i<4; i++) begin : gen_inst0
      // Here, AUTOINST line indents incorrectly indents way too far the the right
      // ( below indents after = above, which is incorrect
      subindent s
			  (/*AUTOINST*/
			   // Outputs
			   .y			(y),
			   // Inputs
			   .a			(a));
   end endgenerate

   // Without the '=0', the AUTOINST line indents properly
   generate for (genvar i; i<4; i++) begin : gen_inst1
      subindent s
        (/*AUTOINST*/
	 // Outputs
	 .y				(y),
	 // Inputs
	 .a				(a));
   end endgenerate

endmodule

module subindent (input a, output y);
endmodule
// New File: <indent_genmod.v>
module foo;
   always@(*)
     begin
	if(state==LBT_STATE)
	  begin
             if(something)
	       begin
	       end
	     else
	       begin
	       end
	  end
	else if(state==HS_0_STATE)
	  begin
	  end
	else if(state==STATE_4) 
	  begin
	  end
	else
	  begin
	  end
     end // always@ (*)
endmodule // foo
// New File: <indent_if2.v>
// Issue 559 - nested ifdef with generate indentation bug
module m;

`ifndef DDR_PT_GATE_SIM
generate
/***************** Assertions for DP HM signals ***************************************************************/
for (dp_id=0; dp_id <PDDR_NUM_DP; dp_id++) begin: DPConnectGen

if (PDDR_DP_HV[dp_id]) begin: VGen
`ifdef IO_DDR3
// CR?V connectivity for DP HM
DP_CRN0V15_Connection: assert property (ConnectProp(1'b1, `PHYTOP.CRN0V[dp_id+PGEN_NUM_ADR], `PHYTOP.DPBundleGen[dp_id].u_DpBundle.`DPV_INST.CRN0V15))
else begin
$display ($stime, " ERROR: CRN0V[%0d] to DP HM %0d CRN0V15 Connection Failed", dp_id+PGEN_NUM_ADR, dp_id);

postError();

end // else: !assert property
DP_CRN1V15_Connection: assert property (ConnectProp(1'b1, `PHYTOP.CRN1V[dp_id+PGEN_NUM_ADR], `PHYTOP.DPBundleGen[dp_id].u_DpBundle.`DPV_INST.CRN1V15))
else begin
$display ($stime, " ERROR: CRN1V[%0d] to DP HM %0d CRN1V15 Connection Failed", dp_id+PGEN_NUM_ADR, dp_id);

postError();

end // else: !assert property
end // block: VGen
`endif //  `ifdef IO_DDR3
end // block: DPConnectGen
endgenerate
`endif

endmodule // m
// New File: <indent_ifdef_generate.v>
module test(out);
  output out;
`define wow
`define nest_one
`define second_nest
`define nest_two
`ifdef wow
  initial $display("wow is defined");
  `ifdef nest_one
  initial $display("nest_one is defined");
    `ifdef nest_two
  initial $display("nest_two is defined");
    `else
  initial $display("nest_two is not defined");
    `endif
  `else
  initial $display("nest_one is not defined");
  `endif
`else
   initial $display("wow is not defined");
 `ifdef second_nest
   initial $display("second_nest is defined");
 `else
   initial $display("second_nest is not defined");
 `endif
`endif
endmodule


// Local Variables:
// verilog-auto-lineup: all
// verilog-auto-endcomments: t
// verilog-auto-indent-on-newline: t
// verilog-auto-lineup: all
// verilog-auto-newline: nil
// verilog-case-indent: 2
// verilog-highlight-p1800-keywords: nil
// verilog-indent-begin-after-if: t
// verilog-indent-level: 2
// verilog-indent-level-behavioral: 2
// verilog-indent-level-declaration: 2
// verilog-indent-level-directive: 2
// verilog-indent-level-module: 2
// verilog-minimum-comment-distance: 40
// verilog-tab-always-indent: t
// End:
// New File: <indent_ifdef.v>
module foo;
   initial
     if (cond1) begin
	sig1 <= {4'h0, 4'hc};
	sig2 <= 8'hff;
     end // if (cond1)
endmodule // foo
// New File: <indent_if.v>
module toto (input logic dummy);
   import "DPI-C" pure function real fabs (input real a);
   import "DPI-C" context function real fcons (input real a);
   import "DPI-C" string_sv2c = task string();
   import "DPI-C" int_sv2c = task intsv2c();
   import "DPI-C" context int_sv2c = task intsv2cont();
   logic a; // wrong indentation
endmodule // wrong indentation
// New File: <indent_importfunction.v>
interface simple_bus; // Define the interface
 logic req, gnt;
   logic [7:0] addr, data;
   logic [1:0] mode;
   logic       start, rdy;
endinterface: simple_bus
module memMod(
	      simple_bus a, // Access the simple_bus interface
    input bit clk);
   
   logic  avail;
   // When memMod is instantiated in module top, a.req is the req
   // signal in the sb_intf instance of the simple_bus interface
   always @(posedge clk) 
     a.gnt <= a.req & avail;
endmodule
module cpuMod(simple_bus b, input bit clk);
   always @(b) begin
   end
endmodule
module top;
   logic clk = 0;
   simple_bus sb_intf(); // Instantiate the interface
   memMod mem(sb_intf, clk); // Connect the interface to the module instance
   cpuMod cpu(.b(sb_intf), .clk(clk)); // Either by position or by name
endmodule

// New File: <indent_interface.v>
module foo;
   input a,b;
   
   always @(a) begin
      b <= #10 ~ a;
   end
endmodule // foo

module bar;
   // 
   input a,b;
   
   always @(a) begin // 
      b <= #10 ~  a;

      a;
   end
endmodule // foo
// New File: <indent_linefeed.v>
module soft_rst ( 
		  // System clock and reset
		  input  clk,
		  input  rst_n,
			 
			 // Interface to software land
		  input  req_soft_rst, // Soft reset request
		  output soft_rst_dne, // Soft reset done
			 
			 // Interface to other modules
		  output dma_halt, // Reset pending, halt activity
		  input  tx_quiet, // TX side is dormant
		  input  rx_quiet, // RX side is dormant
		  output soft_rst, // Soft (sync) reset to VC3 side
		  output hs_async_rst_n // Async reset to host side
		  );
   
   reg [1:0] 		 state;
   
   localparam [1:0] IDLE  = 2'h0,
     HALT 		  = 2'h1,
     RST 		  = 2'h2,
     DONE 		  = 2'h3;
   
endmodule // soft_rst
// New File: <indent_lineup_inlists.v>
module test (pci_ack, reg_wr, reg_sel, clk,  rst); 
   input [3:0] pci_ack; 
   input reg_wr;
   input reg_sel; 
   input clk; 
   input rst;
   initial begin
      foo;
      bar;
      x <= y;
      longish <= alsolongish;
   end
   // Only blocking assignments
   initial begin
      lorem = 0;
      ip = 1;
      sum = 2;
   end
   // Only non-blocking assignments
   initial begin
      dolor <= 0;
      sit <= 1;
      amet <= 2;
   end
   // Mix of blocking and non-blocking assignments
   initial begin
      consectetur = 0;
      adipiscing <= 1;
      elit <= 2;
   end

endmodule

// Local Variables:
// verilog-auto-lineup: all
// End:

// New File: <indent_lineup_mode_all.v>
module test (pci_ack, reg_wr, reg_sel, clk,  rst); 
   input [3:0] pci_ack; 
   input reg_wr;
   input reg_sel; 
   input clk; 
   input rst;
   initial begin
      foo;
      bar;
      x <= y;
      longish <= alsolongish;
   end

endmodule

// Local Variables:
// verilog-auto-lineup: assignments
// End:
// New File: <indent_lineup_mode_assignments.v>
module test (pci_ack, reg_wr, reg_sel, clk,  rst); 
   input [3:0] pci_ack; 
   input reg_wr;
   input reg_sel; 
   input clk; 
   input rst;
   initial begin
      foo;
      bar;
      x <= y;
      longish <= alsolongish;
   end

endmodule

// Local Variables:
// verilog-auto-lineup: declarations
// End:

// New File: <indent_lineup_mode_declarations.v>
module test (pci_ack, reg_wr, reg_sel, clk,  rst); 
   input [3:0] pci_ack; 
   input reg_wr;
   input reg_sel; 
   input clk; 
   input rst;
   initial begin
      foo;
      bar;
      x <= y;
      longish <= alsolongish;
   end

endmodule

// Local Variables:
// verilog-auto-lineup: nil
// End:

// New File: <indent_lineup_mode_none.v>
module foo (
input wire a,
input wire b,
output reg z
);

localparam CONST=1;

generate
for (genvar i=0; i<CONST; i++)
inst inst(
.a(a[i]),
.b(b[i]),
.z(z[i])
);
endgenerate

endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_generate_for2.v>
module foo (
input wire a,
input wire b,
output reg z
);

localparam CONST=1;

generate
for (genvar i=0; i<CONST; i++) begin : label
inst inst(
.a(a[i]),
.b(b[i]),
.z(z[i])
);
end : label
endgenerate

endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_generate_for.v>
module foo (
input wire a,
input wire b,
output reg z
);

parameter VAR = 0;

generate
if (VAR)
inst inst(
.a(a),
.b(b),
.z(z)
);
else
inst2 inst2 (
.a(a),
.b(b),
.z(z)
);
endgenerate

endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_generate_if2.v>
module foo (
input wire a,
input wire b,
output reg z
);

parameter VAR = 0;

generate
if (VAR) begin
inst inst(
.a(a),
.b(b),
.z(z)
);
end
else begin
inst2 inst2 (
.a(a),
.b(b),
.z(z)
);
end
endgenerate

endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_generate_if.v>
module foo # (
parameter A = 0,
B = 0,
C = 0,
D = 0
)(
input wire a,
input wire b,
output reg z
);
endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_params2.v>
module foo # (
parameter A = 0,
parameter B = 0,
parameter C = 0,
parameter D = 0
)(
input wire a,
input wire b,
output reg z
);
endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_params.v>
// bug 433 - Check indentation around macro (uvm/ovm/vmm) with curly-brace
//           internals (these look like a constraint)

module m;
if(x)
`ovm_do_with(my,
{ y == 1;
z == 2; });
endmodule
// New File: <indent_macro_braces.v>
// issue 935 - Incorrect indentation after multi-line `define macro
`ifndef _ABC
  `define _ABC

  `define TEMPA 1

// CORRECT INDENTATION

  `define DEF1 { "a" \
                 , "b" \
                 }

    // Incorrect indentation (v1) after multi-line macro definition above

  `define TEMPB (`TEMPA >= 0 ? 0 : 1)

    // Incorrect indentation (v1) *strangely, not yet affected by the >= chars above*

  `define DEF2 { "a" \
                 , "b" \
                 }

                           // Incorrect indentation (v2) *NOW aligns to the
                           // >= sign in `define TEMPB above (which it should not)
                           // Looks like the multi-line macro definition DEF2
                           // above caused this second version of incorrect
                           // indentation*

  `define TEMPC ((`TEMPA + 1_000_000) >= 0 ? 0 : 1)

    // BACK TO Incorrect indentation (v1)!

  `define DEF3 { "a" \
                 , "b" \
                 }

                           // BACK TO Incorrect indentation (v2)! Surprisingly
                           // the >= sign in `define TEMPC macro did not affect
                           // the indentation this time; but the indentation
                           // returned to (v2) as set by that sign in `define TEMPB
`endif

// Local Variables:
// verilog-indent-ignore-multiline-defines: nil
// End:
// New File: <indent_macro_comment.v>
class Driver;
   mailbox mbox;  // <= not highlighted.
   semaphore smtx;
   int id;
   virtual Rx_if Rx;
   
   function new(mailbox mbox, int id, virtual Rx_if.TB Rx);
      this.mbox = mbox;
      this.id = i;
      this.Rx = Rx;
   endfunction // new
endclass // Driver

// New File: <indent_mailbox.v>
module testmodule(
		   input wire [1:0]   pin1,
		   input wire 	      pin2,
		   input wire [1:0]   pin3,
		   input wire 	      pin4,
		   output wire 	      pin5,
		   output wire [10:0] pin6,
		   output reg 	      pin7,
		   output reg [1:0]   pin8
		   );
   initial begin
      $display("alls well that ends well");
   end
endmodule // testmodule


// New File: <indent_modansi.v>
//-*- mode: Verilog; verilog-indent-level:3; indent-tabs-mode: nil; tab-width: 1 -*-

typedef class b4_c;

class i_sb_c extends base_sb_c;
   `uvm_register_cb(i_sb_c, sb_i_cb_c)

   //------------
   int drain_time_ns = 5000;

   //------------

   typedef struct {
      pem_intf_defs::pem_ncb_err_rsp_t  err_rsp;
      int         rcv_time;
   } ncbi_err_rsp_t;

   //------------
   int 		  duax = 5;

   int 		  trwed = 0;

endclass
// New File: <indent_modeln.v>
module foo;
   modport foo_mp;
   modport foo_mp1(a);
   modport foo_mp2(clocking bar_cb);
   a;
endmodule // foo
// New File: <indent_modport.v>
module test;
property p_test;
a |-> b;
endproperty : p_test
assert property (p_test);
a_test : assert property (p_test);
a  = b; // this and following lines are not properly indented
foo;
endmodule // test
// New File: <indent_named_assert.v>
module ovm;
class simple_item extends ovm_sequence_item;
   rand int unsigned addr;
   rand int unsigned data;
   rand int unsigned delay;
   constraint c1 { addr < 16'h2000; }
   constraint c2 { data < 16'h1000; }
   // OVM automation macros for general objects
   `ovm_object_utils_begin(simple_item)
      a = b;
      c = d;
      `ovm_field_int(addr, OVM_ALL_ON)
      `ovm_field_int(data, OVM_ALL_ON)
      `ovm_field_int(delay, OVM_ALL_ON)
   `ovm_object_utils_end
   // Constructor
   function new (string name = "simple_item");
      super.new(name);
   endfunction : new
endclass : simple_item
class mydata extends ovm_object;

   string str;
   mydata subdata;
   int 	  field;
   myenum e1;
   int 	  queue[$];
   `ovm_object_utils(mydata)
   `ovm_object_utils_begin(mydata) //requires ctor with default args
      `ovm_field_string(str, OVM_DEFAULT)
      `ovm_field_object(subdata, OVM_DEFAULT)
      `ovm_field_int(field, OVM_DEC) //use decimal radix
      `ovm_field_enum(myenum, e1, OVM_DEFAULT)
      `ovm_field_queue_int(queue, OVM_DEFAULT)
   `ovm_object_utils_end
   `ovm_object_param_utils_begin(mydata) //requires ctor with default args
     `ovm_field_string(str, OVM_DEFAULT)
      `ovm_field_object(subdata, OVM_DEFAULT)
      `ovm_field_int(field, OVM_DEC) //use decimal radix
      `ovm_field_enum(myenum, e1, OVM_DEFAULT)
      `ovm_field_queue_int(queue, OVM_DEFAULT)
   `ovm_object_utils_end   
endclass
class my_trans extends ovm_sequence_item;
   
   rand  bit [127:0]               data [];
   
   //---> Configuration
   `ovm_object_utils_begin(my_trans)
      `ovm_field_array_int ( data, OVM_ALL_ON)
   `ovm_object_utils_end
   
   function new (string name = "my_trans", ovm_sequencer_base        sequencer = null, ovm_sequence parent_seq = null);
      super.new(name, sequencer, parent_seq);
   endfunction : new
endclass : my_trans
endmodule // ovm

module tt;

   initial begin
      while (1) begin
         `ovm_do_with(aa, {bb == 0;})
	   `ovm_do(cc)
	   `ovm_do(cc)	     
	     end // while (1)
   end // initial begin
   
endmodule // tt

// New File: <indent_ovm.v>
module test();
    submodule #(
        .param1("HI"),
        // Pre-fix, attempting to indent here yielded
        // Scan error: "Unbalanced parenthesis", 50, 1
        ) modname ();
endmodule

// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_param_1645.v>
module example_block
#(
parameter PARAM    = 1,
FOO 	       = 2,
BARFLUG 	       = 4,
G 		       = 5,
)
(// I/O
input     reset_n,
input     clk
input     a, b,
output reg c
);
endmodule

// New File: <indent_param.v>
// bug861/862 - named coverpoint inside/near pre-processor macro confuses indentation
module m;

`ifdef ASSERT_ON
asrt_001: assert property(p_001);
asrt_002: assert property(p_002);
asrt_003: assert property(p_003);
`endif

`ifdef COVER_ON
chk_001: cover property(p_001);
chk_002: cover property(p_002);
chk_003: cover property(p_003);
`endif

endmodule
// New File: <indent_preproc_label.v>


aa;
`__FILE__
  `__LINE__
    `celldefine
      `end_keywords
	`resetall
	  `unconnected_drive
	    `undefineall

`ifdef AA
`else
`endif
`ifndef AA
`elsif FOO
`endif

	      `begin_keywords "FOO"
`undef FOO

`line 2 "xx" 2
`include "YY"
`include <YY>
`pragma endofline
`timescale 10ns/10ps
`define foo bar
// New File: <indent_preproc.v>
module test
  ();

`ifdef SVA
   property check_x;
   @(negedge clk_n) disable iff (!rst_n)
     ;
endproperty

   property check_y;
      @(negedge clk_n) disable iff (!rst_n)
        ;
   endproperty

`endif

endmodule
// New File: <indent_property_bug1817.v>
module foo();
   initial begin
      a;
   end
   
   always @(a) begin
      b;
   end
   task a (a);
      begin
     a 	= f;
a      = f;
 d     <= 89;
 sdf    = sdf;
         adada  => asda;
 d     ->> g;
 aasd  <<<= 3;
                   	 ccc   %= 6;
d     *= b;
g     -= c;
      end
   endtask // a
   
   
   property p_3;
      a      => ##3 !a;        
      a     |=> ##1 !a;
      a     |-> ##2 !a;
   endproperty   
   property p_2;
      @(posedge clk) b |-> ##1 c;
   endproperty
   property p_1;
      @(posedge clk) a |-> b;
   endproperty
   
   initial d;
   
   //   ap_1 assert property (p_1);  FIXME
   //   ap_2 assert property (p_2);
   
   property p_lane_output_change_on_input_change;
      @(negedge test_clk)
        disable iff (ana_byp == 0)
          !$stable(lane_inputs) |-> !$stable(lane_outputs);
   endproperty

// Issue #940 - '=' in |=> , #=#, and [=n] operators should not mis-indent next line of continued expression
property p_nonSequential;
a |-> b[=n] ##0
c;
endproperty

property p_nonOverlapFollowedBy;
a #=#
c;
endproperty

property p_nonBlockingImplication;
a |=> b[*n] ##0
c;
endproperty


endmodule
// New File: <indent_property.v>
module aa;

   int a,b,c;

   initial begin
      randcase
        10 : begin
           a = 1;
        end
	15 : begin
           b = 0;
           c = 5;
	end
      endcase // randcase

   end // initial begin

endmodule // a
// New File: <indent_randcase.v>
module top;
   initial begin
      // Test
      $display("Hello world");
      void'(std::randomize(foo) with {foo < 10;}; );
   end
endmodule // top
/*
      --------------------------
 
It also appears to have indentation problems with the following variation:
 
== The code: ==
*/ 
module top;
   
   initial begin
      // Test
      $display("Hello world");
      assert(std::randomize(foo) with {foo < 10;}; )
	else $error("ERROR randomizing foo");
   end
endmodule // top
/*
 
------------------------------
 
Also tried the following (removed semicolon after closing-curly-brace), and got same result:
*/
 
module top;
   
   initial begin
      // Test
      $display("Hello world");
      void'(std::randomize(foo) with {foo < 10;} );
   end
endmodule // top
// New File: <indent_random.v>
class example_t;
endclass // example_t

class test;
   typedef class example_t;

   // Breaks with (verilog-pretty-declarations)

   virtual function void cmp_core
     (
      input bit [8:0]   max_len,
      input bit         mv, 
      ref example_t algo_cfg,
      ref bit [17:0]    orig_img [], 
      ref bit [15:0]    cmp_img [], 
      example_t algo_cfg,
      input bit         recmp_en = 1'b0,
      output bit [17:0] re_pixel_output_tmp
      );

   endfunction
endclass

// Local Variables:
// verilog-typedef-regexp:"_t$" 
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <indent_reftype.v>
// Issue #1237 -- "end" not indented correctly after replication with variable number
module test;

   initial begin
      string in_str;

      if (in_str == "") begin : empty_string
     a_string = { 5 {" "}};  // all spaces
      end // block: empty_string
   end // initial begin

   initial begin
      string in_str;
      int    width = 5;

      if (in_str == "") begin : empty_string
     a_string = { width {" "}};  // all spaces
   end // block: empty_string
   end // initial begin

   function string a_string (string in_str);
      if (in_str == "") begin : empty_string
     a_string = { width {" "}};  // all spaces
   end // block: empty_string
      endfunction // a_string

endmodule // test
// New File: <indent_replicate.v>
module test;
   input  i_blc, I_cdef;
   reg [par_a -1:0] b,c;

   always @ ( posedge CLK or negedge RSTN ) begin
      if(!RSTN) begin
	 b = {par_a{1'b0}};
      c = {par_a{1'b0}};
   end
      else begin
         b=i_blc+I_cdef;
         c=b;
      end
   end
endmodule // test
// New File: <indent_rep_msg1188.v>
module test (input logic clk,
             input logic a,
             output logic c,
             output byte  d[4]);

always_ff @(posedge clk) begin
if (a == 1'b1) begin
data <= {<<byte{$urandom()}};
c <= data[1] > 8'h0f;
end
end
endmodule // test
// New File: <indent_streaming_op.v>
module foo;

a = { g + c; };
a = c;

typedef struct {
reg r;
ahb_op_t op; // Read, write, etc.
ahb_cycle_type_t cti; // Cycle type for bursts
ahb_incr_type_t incr; // Increment type (for bursts)
bit b;
reg r;
ahb_thingy a;
bit [31:2] addr; // Starting address
bit [3:0] byte_sel; // Byte lane select
int len; // Length of transfer
bit [31:0] data[0:7]; // Write data
} ahb_req_t;

struct {
reg f;
xyzzy b;
};
struct packed {
int a; // ok
};
struct packed signed {
int a; // woops
};
struct packed unsigned {
int a; // woops
};

endmodule // foo

module foo (
input a,
input c,
output d,
);
always @(a) g;



endmodule // foo


// Local Variables:
// verilog-align-typedef-words: ("ahb_thingy" "xyzzy")
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <indent_struct.v>
module foo;
   
   // for each additional in-air txmacphy byte
   task nextTxByte();
      TxByteCnt++;
      TxLastByteTime  = $time;
   endtask // nextTxByte
   task automatic blah();
      t;
   endtask // blah
   function static foo();
      foo  = 1;
   endfunction // foo
   // start counting when txmacphy sees first in-air byte
   task firstTxByte();
      TxByteCnt        = 1;
      TxFirstByteTime  = $time;
      TxLastByteTime   = $time;
   endtask // firstTxByte
   
   // outputs the overall performance of the RX path in Mbps (MBits per second)
   task printRxPerformance();
      integer ibps;
      real    Mbps;
      if( RxByteCnt && systemTop.intMonitor.frgRxedCnt >= 2 ) begin
         ibps  =
                (RxByteCnt*8*1000000000)/(RxLastByteTime-RxFirstByteTime);
         Mbps  = ibps/1000000;
         $display("%t: %s - RX average performance: %fMbps (Mbits/sec)",
                  $time, myName, Mbps );
      end
      else
        $display("%t: %s - Requires >= 2 RX frames in order to measure performance", $time, myName);
      
   endtask // printRxPerformance
   
endmodule // foo

class a;
   virtual function void foo();
      foo  = 2;
   endfunction // void
   extern function void bar();
   function fred();
      aaa;
   endfunction // fred
   
   task foo;
   endtask // endtask
   
   virtual task foo;
   endtask // endtask
   
   generate g;
   endgenerate
   
   covergroup g;
   endgroup // g
   
   property p;
   endproperty
   
   sequence s;
   endsequence // s
   
   clocking c;
   endclocking // c
   
   function f;
   endfunction //
   
   virtual function f;
   endfunction //
   
   protected function f;
   endfunction //
   
endclass // a

class base_test extends uvm_test;
   `uvm_component_utils(base_test)
   typedef virtual my_if my_vif_t;
   // A task definition starting with the virtual keyword should not be
   // detected as a declaration. This issue is seen when an attempt to indent
   // each declaration is done (when the verilog-auto-lineup variable is set
   // to 'declarations).
   //   In other words, the "task" in "virtual task" below must not be
   // aligned with "my_if" in the "typedef virtual my_if.." line above.
   virtual task run_phase(uvm_phase phase);
      super.run_phase(phase);
   endtask // run_phase
endclass // base_test
// New File: <indent_task.v>
module  xxx_xxxxxx  (input wire clk, input wire reset);
   
   typedef enum reg [4:0] {IDLE, IIII, HHHHH,
			   AA_OP, AA_CMRD, AA_CMRD_WAIT, AA_RMW, AA_RMW_WAIT, AA_CMLLL, AA_CMLLL_WAIT, AA_NEXT,
			   BB_OP, BB_CMLLL, BB_CMLLL_WAIT, BB_NEXT,
			   CC_OP, CC_NEXT_OOOO,
			   DD_OP, DD_CMRD, DD_CMRD_WAIT, DD_ACCUM, DD_CMLLL, DD_CMLLL_WAIT,
			   EE_OP, EE_NEXT_OOOO,
			   FF_OP,
			   zxcvzxcv, cvbncvbn} xxxxxx_state_e;
   
   xxxxxx_state_e current_state;

   always_ff @ (posedge clk) begin
      if (reset) begin
	 current_state <=  IDLE;
      end
      else begin
	 unique case (current_state)
	   
	   IDLE : begin
              qwerty <=  '0;
	      
              if (~qqq_empty)
		current_state <=  HHHHH;
	   end
	 AA_CMLLL : begin
	    lll_start <=  1'b1;
	    
	    if (jjjj_left < 4)
	      lll_wcnt <=  jjjj_left[2:0];
	    else
	      lll_wcnt <=  FOUR[2:0];
	    
	    current_state <=  AA_CMLLL_WAIT;
	 end
	 
	 HHHHH : begin
            qqq_opiuy <=  1'b1;
	    
            if (qqq_opiuy) begin
	       qqq_opiuy <=  '0;
	       current_state <=  IIII;
	       
            end
	 end
	 
	 AA_OP : begin
            if (aa_pgm_err) begin
	       current_state <=  zxcvzxcv;
            end
            else begin
	       jjjj_left <=  tgbyhn;
	       
	       current_state <=  AA_CMRD;
            end
	 end
	 
	 AA_CMRD : begin
	    uuuuu <=  1'b1;
	    current_state <=  AA_CMRD_WAIT;
	 end
	 IIII : begin
            qqq_opiuy <=  '0;
            if (err_iiii) begin
	       current_state <=  zxcvzxcv;
            end
            else begin
	       unique0 case (opm_cur)
		 
		 `XXXCP : current_state <=  AA_OP;
		 `XXXZR : current_state <=  BB_OP;
		 
		 default : current_state <=  zxcvzxcv;
	       endcase // unique0 case
	       
            end // else: !if(err_iiii)
	 end // case: IIII
	 
	 AA_CMRD_WAIT : begin
	    uuuuu <=  '0;
	    if (kjkjkjkjk) begin
	       if (err_cmrd_par) begin
		  current_state <=  zxcvzxcv;
	       end
	       else begin
		  if (err_cmrd_csel) begin
		     current_state <=  zxcvzxcv;
		  end
		  else begin : assign_writecvbn
		    lllcvbn <=  asdf ? ghjk : rdcvbn;
		     lll_par <=  asdf ? cvbn : rd_par;
		     
		     current_state <=  AA_CMLLL;
		  end
	       end // else: !if(err_cmrd_par)
	    end // if (kjkjkjkjk)
	 end // case: AA_CMRD_WAIT
	 
	 AA_CMLLL_WAIT : begin
	    lll_start <=  '0;
	    
	    if (lll_done) begin
	       if (alalala) begin
		  current_state <=  zxcvzxcv;
	       end
	       else begin
		  current_state <=  AA_NEXT;
	       end
	    end
	 end // case: AA_CMLLL_WAIT
	 
	 
	 
	 AA_NEXT : begin
	    
            if (qwerty) begin
               qwerty <=  '0;
	       
               
               unique case (opm_cur)
		 
		 `XXXCP : current_state <=  cvbncvbn;
		 `XXXSG : current_state <=  CC_NEXT_OOOO;
		 default : current_state <=  zxcvzxcv;
               endcase // unique  case
               
            end // if (qwerty)
            else begin
               jjjj_left <=  jjjj_left - 4;
	       
               current_state <=  AA_CMRD;
            end // else: !if(qwerty)
         end // case: AA_NEXT
	 
         BB_OP : begin
            if (bb_pgm_err) begin
               current_state <=  zxcvzxcv;
            end
            else begin
               lllcvbn <=  '0;
               lll_par <=  '0;
               jjjj_left <=  tgbyhn;
               
               current_state <=  BB_CMLLL;
            end
         end // case: BB_OP
         
         
	 
         BB_CMLLL : begin
            lll_start <=  1'b1;
	    
            if (jjjj_left <= 4) begin
               lll_wcnt <=  jjjj_left[2:0];
               qwerty <=  1'b1;
            end
            else begin
               lll_wcnt <=  FOUR[2:0];
            end
            
            current_state <=  BB_CMLLL_WAIT;
         end // case: BB_CMLLL
	 
         
         
         BB_CMLLL_WAIT : begin
            lll_start <=  '0;
            
            if (lll_done) begin
               if (alalala) begin
                  current_state <=  zxcvzxcv;
               end
               else begin
                  current_state <=  BB_NEXT;
               end
            end
         end // case: BB_CMLLL_WAIT
         
         
	 
         
         BB_NEXT : begin
            if (qwerty) begin
               qwerty <=  '0;
               current_state <=  cvbncvbn;
            end
            else begin
               jjjj_left <=  jjjj_left - 4;
               current_state <=  BB_CMLLL;
            end
         end
	 
	 
	 
	 
         
         
         CC_OP : begin
            jjjj_left_oooo <=  tgbyhn;
            
            if (tgbyhn <= oooo_cur) begin
               last_oooo <=  1'b1;
               jjjj_left <=  tgbyhn;
            end
            else begin
               jjjj_left <=  oooo_cur;
            end
	    
            current_state <=  AA_CMRD;
            
         end // case: CC_OP
         
         
	 
         
         CC_NEXT_OOOO : begin
            if (last_oooo) begin
               current_state <=  cvbncvbn;
            end
            else begin
	       
               rd_rrrr <=  rd_rrrr + ttttt_cur;
               lll_rrrr <=  lll_rrrr + oooo_cur;

               
               if (jjjj_left_oooo <= oooo_cur) begin
                  last_oooo <=  1'b1;
                  jjjj_left <=  jjjj_left_oooo;
               end
               else begin
                  jjjj_left <=  oooo_cur;
               end

               current_state <=  AA_CMRD;
            end // else: !if(last_oooo)
            
         end // case: CC_NEXT_OOOO


         
         
         
         DD_OP : begin
            accumulate_sum <=  '0;
            jjjj_left <=  tgbyhn;

            current_state <=  DD_CMRD;
         end
         
         
         DD_CMRD : begin
            uuuuu <=  1'b1;

            if (jjjj_left <= 4) begin
               qwerty <=  1'b1;
            end

            current_state <=  DD_CMRD_WAIT;
         end


         
         DD_CMRD_WAIT : begin
            uuuuu <=  '0;

            if (kjkjkjkjk) begin
               if (zazaz) begin
                  current_state <=  zxcvzxcv;
               end
               else begin

                  current_state <=  DD_ACCUM;
               end
            end
         end // case: DD_CMRD_WAIT
         
         
         DD_ACCUM : begin
            if (qwerty) begin
               current_state <=  DD_CMLLL;
            end
            else begin
               current_state <=  DD_CMRD;
            end
         end
         

         
         DD_CMLLL : begin
            lll_start <=  1'b1;
            
            current_state <=  DD_CMLLL_WAIT;
         end

         
         
         DD_CMLLL_WAIT : begin
            lll_start <=  '0;
            
         end
         

         
         
         EE_OP : begin
            jjjj_left_oooo <=  tgbyhn;
            
            current_state <=  AA_CMRD;
         end


         
         
         EE_NEXT_OOOO : begin
            if (last_oooo) begin
               current_state <=  cvbncvbn;
            end
            else begin

            end
         end

         
         
         FF_OP : begin
            asdf <=  1'b1;

            current_state <=  CC_OP;
         end
         
         zxcvzxcv : begin
            current_state <=  cvbncvbn;
         end
         
         cvbncvbn : begin
            if (dci_cur) begin
               current_state <=  IDLE;
               cmd_proc_done <=  1'b1;
            end
            else if (crq_ready) begin
               crq_start <=  1'b1;
               crqcvbn <=  complcvbn;
               crq_proc_id <=  proc_id_cur;
	       
               current_state <=  IDLE;
               cmd_proc_done <=  1'b1;
            end
         end // case: cvbncvbn
         
         default : begin
            current_state <=  IDLE;
            cmd_proc_done <=  1'b1;
         end
      endcase // unique  case
      end // else: !if(reset)
   end // always _ff



endmodule // xxx_xxxxxx





// Local Variables:
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_e\\>"
// End:
// New File: <indent_unique_case-1.v>
module testmod ();
   always_comb begin
      unique case (eeee)
        ZERO[1:0] : begin
           a = 1;
        end // case: ZERO[1:0]
	
	ONE[1:0] : begin
           a = 1;
	end // case: ONE[1:0]
	
	TWO[1:0] : begin
           a = 1;
	end // case: TWO[1:0]
	THREE[1:0] : begin
           a = 1;
	end // case: THREE[1:0]
      endcase // unique case (eeee)
   end // always_comb
   
   always_ff @ (posedge clk) begin
      if (reset) begin
         current_state <= `TQ STATE0;
      end // if (reset)
      else begin
         priority case (current_state)
           STATE0 : begin
              current_state <= `TQ STATE3;
           end // case: STATE0
	   
           STATE1 : begin
              current_state <= `TQ STATE3;
           end // case: STATE1
	   
           STATE2 : begin
              current_state <= `TQ STATE3;
           end // case: STATE2
	   
           STATE3 : begin
              current_state <= `TQ STATE0;
           end // case: STATE3
	   
           default : current_state <= `TQ STATE0;
	 endcase // priority case (current_state)
      end // else: !if(reset)
   end // always_ff @

endmodule // testmod




// New File: <indent_unique_case-2.v>
module foo;
   // syntaxify the unique keyword correctly please...
   always_comb (*) begin
      case (f)
	1 		  : 2;
      endcase // case (f)
      
      unique case(vlcnum)
	0 : unique case(in.value1)
		     0 	  : out = 1; 1 : out = 3; 2 : out = 3; 3 : out = 4;
		     4 	  : out = 4; 5 : out = 5; 6 : out = 5; 7 : out = 6;
		     8 	  : out = 6; 9 : out = 7; 10: out = 7; 11: out = 8;
		     12 	  : out = 8; 13: out = 9; 14: out = 9; 15: out = 9;
		   endcase
        1 	  :
	  unique
	  case(in.value1)
	    0 		  : out = 3;
	    1 	  : out = 3;
	    2 	  : out = 3;
	    3 	  : out = 3;
	    4 	  : out = 3;
	    5 	  : out = 4;
	    6 	  : out = 4;
	    7 	  : out = 4;
	    8 	  : out = 4;
	    9 	  : out = 5;
	    10   : out = 5;
	    11   : out = 6;
	    12   : out = 6;
	    13   : out = 6;
	    14   : out = 6;
	  endcase // case (in.value1)
      endcase // case (in.value1)
   end
endmodule
// New File: <indent_unique_case.v>
module uvm;
class simple_item extends uvm_sequence_item;
   rand int unsigned addr;
   rand int unsigned data;
   rand int unsigned delay;
   constraint c1 { addr < 16'h2000; }
   constraint c2 { data < 16'h1000; }
   // UVM automation macros for general objects
   `uvm_object_utils_begin(simple_item)
      a = b;
      c = d;
      `uvm_field_int(addr, UVM_ALL_ON)
      `uvm_field_int(data, UVM_ALL_ON)
      `uvm_field_int(delay, UVM_ALL_ON)
   `uvm_object_utils_end
   // Constructor
   function new (string name = "simple_item");
      super.new(name);
   endfunction : new
endclass : simple_item
class mydata extends uvm_object;

   string str;
   mydata subdata;
   int 	  field;
   myenum e1;
   int 	  queue[$];
   `uvm_object_utils(mydata)
   `uvm_object_utils_begin(mydata) //requires ctor with default args
      `uvm_field_string(str, UVM_DEFAULT)
      `uvm_field_object(subdata, UVM_DEFAULT)
      `uvm_field_int(field, UVM_DEC) //use decimal radix
      `uvm_field_enum(myenum, e1, UVM_DEFAULT)
      `uvm_field_queue_int(queue, UVM_DEFAULT)
   `uvm_object_utils_end
   `uvm_object_param_utils_begin(mydata) //requires ctor with default args
     `uvm_field_string(str, UVM_DEFAULT)
      `uvm_field_object(subdata, UVM_DEFAULT)
      `uvm_field_int(field, UVM_DEC) //use decimal radix
      `uvm_field_enum(myenum, e1, UVM_DEFAULT)
      `uvm_field_queue_int(queue, UVM_DEFAULT)
   `uvm_object_utils_end   
endclass
class my_trans extends uvm_sequence_item;
   
   rand  bit [127:0]               data [];
   
   //---> Configuration
   `uvm_object_utils_begin(my_trans)
      `uvm_field_array_int ( data, UVM_ALL_ON)
   `uvm_object_utils_end
   
   function new (string name = "my_trans", uvm_sequencer_base        sequencer = null, uvm_sequence parent_seq = null);
      super.new(name, sequencer, parent_seq);
   endfunction : new
endclass : my_trans
endmodule // uvm

module tt;

   initial begin
      while (1) begin
         `uvm_do_with(aa, {bb == 0;})
	   `uvm_do(cc)
	   `uvm_do(cc)	     
	     end // while (1)
   end // initial begin
   
endmodule // tt

// New File: <indent_uvm.v>
`include "x.h"
//
module x;
   ////
   reg             y;
endmodule // x
// New File: <indent_warren.v>
module ex_inject (i, o);
   input i;
   input j;
   output o;

   // Ok:
   always @ (j or i)
     o = i | j;
   // No change:
   always @ (j) o = i | j;
   always @ (j or i or p) o = i | j;
   // Instant

   autoinst_lopaz_srpad pad
     (.pin(pin),
      .foo(bar),
      .clk(newclk));

endmodule
// New File: <inject_first.v>
module inject_inst_empty_ports;
  logic [7:0] q;
  logic [7:0] d;
  logic       clk, rst_n;

  register r1 (.q(q),
               .qb(), // unconnect output
               .d(d),
               .clk(clk),
               .rst_n(rst_n)
               );
endmodule

module register (
  output logic [7:0] q, qb,
  input  logic [7:0] d,
  input  logic       clk, rst_n
  );

  always_ff @(posedge clk or negedge rst_n)
    if (!rst_n) q <= '0;
    else        q <= d;

  assign qb = ~q;
endmodule
// New File: <inject_inst_empty_ports.v>
module inject_inst_endparen;
  logic [7:0] q;
  logic [7:0] d;
  logic       clk, nreset;

  register r1 (.q(q),
               .d(d),
               .clk(clk),
               // This is the
               // asynchronous reset
               .rst_n(nreset)
               );

`if NEVER
  register r1 (.q(qKEEP),
               .d(dKEEP),
               .clk(clkKEEP),
               // This is the
               // asynchronous reset
               .rst_n(nreset)
               );
`endif

endmodule

module register (
  output logic [7:0] q,
  input  logic [7:0] d,
  input  logic       clk, rst_n
  );

  always_ff @(posedge clk or negedge rst_n)
    if (!rst_n) q <= '0;
    else        q <= d;
endmodule
// New File: <inject_inst_endparen.v>
module inject_inst_net_case (
  output logic [7:0] q2,
  input  logic [7:0] d,
  input  logic       clk, rst_n
  );
  logic [7:0] Q1;

  register2 r2 (.q2(q2), .q1(Q1), .ck(clk), .rst_n(rst_n));

  register1 r1 (.q1(Q1), .d(d),   .ck(clk), .rst_n(rst_n));
endmodule

module register2 (
  output logic [7:0] q2,
  input  logic [7:0] q1,
  input  logic       ck, rst_n
  );
  
  always_ff @(posedge ck or negedge rst_n)
    if (!rst_n) q2 <= '0;
    else        q2 <= q1;
endmodule

module register1 (
  output logic [7:0] q1,
  input  logic [7:0] d,
  input  logic       ck, rst_n
  );
  
  always_ff @(posedge ck or negedge rst_n)
    if (!rst_n) q1 <= '0;
    else        q1 <= d;
endmodule
// New File: <inject_inst_net_case.v>
module inject_inst_param;
  parameter WIDTH = 8;
  logic [WIDTH-1:0] q;
  logic [WIDTH-1:0] d;
  logic             clk, rst_n;

  register #(.WIDTH(WIDTH)) r1 (.q(q), .d(d), .clk(clk), .rst_n(rst_n));
endmodule

module register #(parameter WIDTH=4) (
  output logic [WIDTH-1:0] q,
  input  logic [WIDTH-1:0] d,
  input  logic             clk, rst_n
  );
  
  always_ff @(posedge clk or negedge rst_n)
    if (!rst_n) q <= '0;
    else        q <= d;
endmodule
// New File: <inject_inst_param.v>
module comments_test;
  logic [7:0] q1;
  logic [7:0] d;
  logic       clk, rst_n;

  register r1 (.q(q1),
               .d(d),
               // 100MHz clk signal
               .clk100(clk),
               // Asynchronous reset
               .rst_n(rst_n)
               );
endmodule

// Local Variables:
// mode: Verilog
// verilog-library-flags:("-f inject_path.f")
// End:
// New File: <inject_path_cmt.v>
// Note module names don't match, this is intentional to check the .f file

module reg_core (
  output logic [7:0] q,
  input  logic [7:0] d,
  input  logic       clk, rst_n
  );

  always_ff @(posedge clk or negedge rst_n)
    if (!rst_n) q <= '0;
    else        q <= d;
endmodule

module register (
  output logic [7:0] q,
  input  logic [7:0] d,
  input  logic       clk, rst_n
  );
  
  reg_core c1 (.q(q), .d(d), .clk(clk), .rst_n(rst_n));
endmodule
// New File: <inject_path_sub.v>
module inject_path;
  logic [7:0] q;
  logic [7:0] d;
  logic       clk, rst_n;

  register r1 (.q(q), .d(d), .clk(clk), .rst_n(rst_n));
endmodule

// Local Variables:
// mode: Verilog
// verilog-library-flags:("-f inject_path.f")
// End:
// New File: <inject_path.v>
module inst (/*AUTOARG*/
   // Outputs
   lower_out,
   // Inputs
   lower_inb, lower_ina
   );

   parameter param1;
   parameter param2;

   input lower_inb;
   input lower_ina;
   output lower_out;

   wire   lower_out = lower_ina | lower_inb;

endmodule

// New File: <inst.v>
// bug 895 - labeling of always constructs
module example;

always
begin
end

always @(posedge clk)
begin
end

always @*
begin
end

always @(*)
begin
end // always @ (*)

always_comb
begin
end

always_latch
begin
end

always @( *)
begin
end

always @(* )
begin
end

always_ff @(posedge clk)
begin
end

always @(*)
begin
end // garbage

endmodule

// Local Variables:
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// End:
// New File: <label_always.v>
module mymodule();
    parameter real fc=10e6;
  parameter real bw=25e3;

analog begin
// contents of module here
   end

     endmodule

// Local Variables:
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// End:
// New File: <label_analog.v>
// bug1259

class trial_good; // this works because the variable class_name has been renamed clazz_name
   string clazz_name="trial_good";
   function void f();
   endfunction // f
endclass // trial_good

class trial_bad; // this fools the autocommenter, that ends reporting "function" as class name
   string class_name="trial_bad";
   function void f();
   endfunction // f
endclass // trial_bad

// Local Variables:
// verilog-auto-endcomments: t
// End:
// New File: <label_class2.v>

connectmodule Wreal2L(input wreal w, output wire l);
logic lval;

  parameter real vsup = 1.8;
       parameter real vth_hi = 0.7*vsup;
  parameter real vth_lo = 0.3*vsup;
  always begin
if( w >= vth_hi) lval = 1'b1;
    else if(w <= vth_lo) lval = 1'b0;
    else if(w === `wrealZState) lval = 1'bz;
    else lval = 1'bx;
    @(w);
end

assign l = lval;
   endconnectmodule

// Local Variables:
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// End:

// New File: <label_connectmodule.v>
// bug 842
module x;

do begin
end while();

endmodule

// Local Variables:
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// End:
// New File: <label_do.v>
module test();

  // function with no lifetime, return-type, or port-list
  function f0;
    blah0;
  endfunction

  // empty port-list
  function f1();
    blah1;
  endfunction

  // non-empty portlist
  function f2(stuff2);
    blah2;
  endfunction

  // test that ": function_identifier" remains unscathed
  function f3;
  endfunction : f3

  // return type
  function void f4;
    blah4;
  endfunction

  // return type with empty port-list.
  function void f5();
    int i;
    begin
      blah4;
    end
  endfunction

  // return type, non-empty portlist
  // also check that a stale auto-comment gets removed
  function void f6(stuff,
                   that,
                   spans,
                   lines);
    blah5;
  endfunction // fX

  // test lifetime keywords 'automatic' and 'static'
  function automatic f7();
  endfunction

  // test a crazy-long function declaration
  function static union packed signed {bit[1:0] a, bit[2:0] b} [5:0] f8(input ports, input ports, output ports);
  endfunction

  // port-list that doesn't start on the same line as the function declaration
  function automatic void f9
    (int a,
     int b);
  endfunction

  // mismatched keyword
  function f10;
  endtask

  // make sure previous screw-up doesn't affect future functions
  function f11;
  endfunction

endmodule
// New File: <label_function.v>
// Bug 859 -- 'end' should label after a user/library-defined macro
// that isn't terminated with a semi-colon
class c;

`uvm_info(get_type_name(), "Digital power up.", UVM_MEDIUM)

fork
begin
end
join

`made_up_macro (a, \
                b, \
                c)
begin: named_block
end

endclass

// Local Variables:
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// verilog-indent-ignore-multiline-defines: nil
// End:
// New File: <label_macro.v>
// bug 841
module x;

always @*
begin
end

endmodule

// Local Variables:
// verilog-indent-level-module: 0
// verilog-indent-begin-after-if: nil
// verilog-minimum-comment-distance: 1
// verilog-auto-endcomments: t
// End:
// New File: <label_no_indent.v>
module test();

  // task with no lifetime, return-type, or port-list
  task t0;
    blah0;
  endtask

  // empty port-list
  task t1();
    blah1;
  endtask

  // non-empty portlist
  task t2(stuff2);
    blah2;
  endtask

  // test that ": task_identifier" remains unscathed
  task t3;
  endtask : t3

  // check that stale auto-label is overwritten
  task t4(input port1, output port2, inout port3);
    begin
      blah blah blah;
    end
  endtask // tX

  // test lifetime keywords 'automatic' and 'static'
  task static t7();
  endtask

  // test a more complete example
  task automatic  t8(input ports, input ports, output ports);
  endtask

  // port-list that doesn't start on the same line as the task declaration
  task automatic t9
    (int a,
     int b);
  endtask

  // mismatched keyword
  task t10;
  endfunction

  // make sure even the simplest test works after all the insanity
  task t11;
  endtask

endmodule
// New File: <label_task.v>
module lavigne_instpath ();

   /* lavigne_t1 AUTO_TEMPLATE (
    .a(a1),
    );
    */
   lavigne_t1 m1 (/*AUTOINST*/
		  // Inputs
		  .a			(a1));			 // Templated

   /* lavigne_t2 AUTO_TEMPLATE (
    .b(b2),
    );
    */

   lavigne_t2 m2 (/*AUTOINST*/
		  // Inputs
		  .b			(b2));			 // Templated

endmodule

// verilkkkkkkkog-library-directories:("*/")

//(load-file "/usr/local/common/site-lisp/local/verilog-mode.el")

// Local Variables:
// verilog-library-flags:("-y lavigne_instpath/")
// End:
// New File: <lavigne_instpath.v>
module ocnSwitchClockGen(output reg ocnSwitchClock);
  parameter   ocnSwitchClockPeriod  = 2000;
   // New File: <lineup.v>
//From: Michael McNamara <mac@verilog.com>
//Date: Tue, 27 Jun 2000 04:38:32 -0700 (PDT)
module x;

   always @ (/*AS*/top.agc_tool.adc_dta_i or top.agc_tool.adc_preagc_dta_i) begin
      agctoolerr = top.agc_tool.adc_dta_i / top.agc_tool.adc_preagc_dta_i;
   end

endmodule
endmodule
// New File: <mac_autosense_dot.v>
module foobar;
   
   sequence s_1;
      {txd, txdk} && (ppd == 3'b000) && !txe;
   endsequence // s_1
  
   sequence s_phy_transmitting;
      {rxd, rxdk} && 
	((ppd == 3'b000) || (ppd == 3'b001))
	&& rxv;
   endsequence // s_phy_transmitting
   
  
endmodule
// New File: <mac_test2.v>
module InstName (/*AUTOARG*/
   // Outputs
   out,
   // Inputs
   in
   );
   input [7:0] in;
   output [7:0] out;
   wire [7:0] 	out = ~in;
endmodule // bottm

module top (/*AUTOARG*/
   // Outputs
   outgo,
   // Inputs
   incom
   );
   input [31:0] incom;
   output [31:0] outgo;

   /* AUTO_LISP(defun getparam2 (strg)
    (string-match "[^0-9]*[0-9]+[^0-9]*\\([0-9]+\\)" strg)
    (match-string 1 strg))*/
   /* InstName AUTO_TEMPLATE (
    .out (@),
    .in (@"(getparam2 vl-cell-name)"),
    );
    */

   InstName BT0_2 (/*AUTOINST*/
		   // Outputs
		   .out			(0),			 // Templated
		   // Inputs
		   .in			(2));			 // Templated
   InstName BT1_3 (/*AUTOINST*/
		   // Outputs
		   .out			(1),			 // Templated
		   // Inputs
		   .in			(3));			 // Templated
   InstName BT2_5 (/*AUTOINST*/
		   // Outputs
		   .out			(2),			 // Templated
		   // Inputs
		   .in			(5));			 // Templated
   InstName BT3_9 (/*AUTOINST*/
		   // Outputs
		   .out			(3),			 // Templated
		   // Inputs
		   .in			(9));			 // Templated

endmodule



// New File: <more_params.v>
`define XYZ            4'ha

module test ();

   always @(/*AUTOSENSE*/r or x)
     begin
	casex(x)
	  5: d = {r, `XYZ };
	endcase
     end

endmodule // test

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <morrison.v>
module test(out);
   output out;
`define wow
`define nest_one
`define second_nest
`define nest_two
`ifdef wow
   initial $display("wow is defined");
 `ifdef nest_one
   initial $display("nest_one is defined");
  `ifdef nest_two
   initial $display("nest_two is defined");
  `else
   initial $display("nest_two is not defined");
  `endif
 `else
   initial $display("nest_one is not defined");
 `endif
`else
   initial $display("wow is not defined");
 `ifdef second_nest
   initial $display("second_nest is defined");
 `else
   initial $display("second_nest is not defined");
 `endif
 `ifdef abc
   initial $display("second_nest is not defined");
 `endif
 `ifdef abc
   initial $display("second_nest is not defined");
 `endif
`endif
endmodule


module test(out);
   output out;
`define wow
`define nest_one
`define second_nest
`define nest_two
`ifdef wow
   initial $display("wow is defined");
 `ifdef nest_one
   initial $display("nest_one is defined");
  `ifdef nest_two
   initial $display("nest_two is defined");
  `else
   initial $display("nest_two is not defined");
  `endif
 `else
  `ifdef abc
   initial $display("nest_one is not defined");
  `else
   initial $display("nest_one is not defined");
  `endif
 `endif
`else
   initial $display("wow is not defined");
 `ifdef second_nest
   initial $display("second_nest is defined");
 `elsif cced
   initial $display("second_nest is defined");
 `elsif cced
  `ifdef abc
   initial $display("second_nest is defined");
  `elsif abc1
   initial $display("second_nest is defined");
  `else
   initial $display("second_nest is defined");
  `endif
  `ifdef abc
   initial $display("second_nest is defined");
  `endif
 `elsif abc11
   initial $display("second_nest is defined");
 `else
   initial $display("second_nest is not defined");
 `endif
`endif
endmodule
// New File: <NestedConditionalCompilationDirective.v>
// Tests that final ); has right indent

module autoinst_autonohookup (
                              /*AUTOINPUT*/
                              // Beginning of automatic inputs (from unused autoinst inputs)
                              input           i1,                     // To a2 of a2.v
                              // End of automatics
                              /*AUTOOUTPUT*/
                              // Beginning of automatic outputs (from unused autoinst outputs)
                              output          o1                     // From a2 of a2.v
                              // End of automatics
                              );
   /* a2 AUTO_TEMPLATE (
    .i1(i1),
    .o1(o1),
    ) */
   a2 a2(/*AUTOINST*/
         // Outputs
         .o1                            (o1),                    // Templated
         // Inputs
         .i1                            (i1));                   // Templated
endmodule

module a2 (
           input  i1,
           output o1
           );
endmodule

// New File: <noindent_autoinst_templated.v>
module testMultiLineParams (/*AUTOARG*/) ;
`include "params_multiline_msg618_inc.vh"
   reg comboIn, comboOut;
   
   // expect AUTOSENSE to be comboIn only, no params
   always @ ( /*AUTOSENSE*/comboIn) begin
      comboOout = param1 & param2 & param3 & comboIn;
   end
   
endmodule // foo

// Local Variables:
// verilog-library-directories:(".")
// verilog-auto-read-includes:t
// End:
// New File: <params_multiline_msg618.v>

//////////////////////////////////////////////////////////////////////////////
//
//  Main
//
//////////////////////////////////////////////////////////////////////////////


module test();

   integer                    count;
   bit                        test_clk;
   
   
   // Create a test clock
   always #01.8 test_clk = ~test_clk;
   
   //**********************************************************************
   // Testing.
   // Shift a moving set of ones up the input vector. At each shift
   // the outputs should change, which is checked by the assertions
   // below. This test doesnt care which output changes, as that was
   // checked to be accurate by formal means.
   //**********************************************************************
   
   initial begin
      count=0;
   end
   
   always @(posedge test_clk) begin
      count++;
   end
   
   //**********************************************************************
   // SV assertions
   //**********************************************************************
   property p_lane_output_change_on_input_change;
      @(negedge test_clk)
        disable iff (ana_byp == 0)
          !$stable(lane_inputs) |-> !$stable(lane_outputs);
   endproperty
   
   a_lane_output_change_on_input_change: assert property (p_lane_output_change_on_input_change)
     else begin
        $error("ERROR! Analog Bypass: Input change not observed on the outputs: %h (lane)",
               lane_inputs);
     end // UNMATCHED !!
   endproperty //FIXME

   property p_sup_output_change_on_input_change;
      @(negedge test_clk)
        disable iff (ana_byp == 0)
          !$stable(sup_inputs) |-> !$stable(sup_outputs);
   endproperty
   
   a_sup_output_change_on_input_change: assert property (p_sup_output_change_on_input_change)
     else begin
        $error("ERROR! Analog Bypass: Input change not observed on the outputs: %h (sup)",
               sup_inputs);
     end
endproperty
endmodule // test
// New File: <property_test.v>
module x;
   parameter Pp = 1;
   /*AUTOINSERTLISP(insert vh-Pp "\n")*/

   localparam Lp = 2;
   /*AUTOINSERTLISP(insert vh-Lp "\n")*/

   parameter mytype_t Pt = 3;
   /*AUTOINSERTLISP(insert vh-Pt "\n")*/

endmodule

// Local Variables:
// verilog-library-directories: (".")
// eval:(verilog-read-defines)
// End:
// New File: <read_define_param.v>
module lbnsub
  (/*AUTOARG*/
   // Outputs
   outgo,
   // Inputs
   income
   );

   input	[7:4]	income;
   output [7:4] 	outgo;

   wire [7:4] 		outgo = income;

endmodule

module lbm
  (/*AUTOARG*/
   // Outputs
   outgo,
   // Inputs
   income
   );

   input	[7:4]	income;
   output [7:4] 	outgo;

   /*
    lbnsub AUTO_TEMPLATE (
    // Inputs
    .income	(income[4:7]));
    */

   lbnsub lbnsub	(/*AUTOINST*/
			 // Outputs
			 .outgo			(outgo[7:4]),
			 // Inputs
			 .income		(income[4:7]));	 // Templated
endmodule

// New File: <reversed_bits.v>
// HERE IS THE TEST CASE
module bt (/*AUTOARG*/
   // Outputs
   out,
   // Inputs
   in
   );
   input [7:0] in;
   output [7:0] out;

   wire [7:0] 	out = ~in;
endmodule // bottm

module top (/*AUTOARG*/
   // Outputs
   outgo,
   // Inputs
   incom
   );
   input [31:0] incom;
   output [31:0] outgo;

   /* bt  AUTO_TEMPLATE (
    .in (incom[@"(+ (* 8 @) 7)":@"(* 8 @)"]),
    .out (outgo[@"(concat (int-to-string (+ (* 8 @) 7)) \":\" (int-to-string ( * 8 @)))"]));
    */

   bt BT0 (/*AUTOINST*/
	   // Outputs
	   .out				(outgo[7:0]),		 // Templated
	   // Inputs
	   .in				(incom[7:0]));		 // Templated
   bt BT1 (/*AUTOINST*/
	   // Outputs
	   .out				(outgo[15:8]),		 // Templated
	   // Inputs
	   .in				(incom[15:8]));		 // Templated
   bt BT2 (/*AUTOINST*/
	   // Outputs
	   .out				(outgo[23:16]),		 // Templated
	   // Inputs
	   .in				(incom[23:16]));		 // Templated
   bt BT3 (/*AUTOINST*/
	   // Outputs
	   .out				(outgo[31:24]),		 // Templated
	   // Inputs
	   .in				(incom[31:24]));		 // Templated

endmodule // top



// New File: <singh.v>
module x (/*AUTOARG*/
   // Inputs
   MIERHW, MBOOTH_P, CEIopMADH_E_D2_R, CEIopMAZH_E_D2_R, DDATAH, DIV2HI, HI_R, MCLA, MCLASH,
   MULTSHCYC, MULTUSCYC, HI_P
   );

   input [18:0] MIERHW;
   integer 	i;
   integer 	MTEMP1;
   integer 	MTEMP2;
   input 	MBOOTH_P;
   input 	CEIopMADH_E_D2_R;
   input 	CEIopMAZH_E_D2_R;
   input 	DDATAH;
   input 	DIV2HI;
   input 	HI_R;
   input 	MCLA;
   input 	MCLASH;
   input 	MULTSHCYC;
   input 	MULTUSCYC;
   input 	HI_P;

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   // End of automatics

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   // End of automatics

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   // End of automatics

   always @(/*AUTOSENSE*/MIERHW) begin

      for (i=0; i<=5; i=i+1) begin

	 MTEMP1[3:0] = {MIERHW[i*3+3],
			MIERHW[i*3+2],
			MIERHW[i*3+1],
			MIERHW[i*3+0]};

	 casex (MTEMP1)

           4'b0000: MTEMP2 = 4'b0101; // +0
           4'b0001: MTEMP2 = 4'b0001; // +1
           4'b0010: MTEMP2 = 4'b0001; // +1
           4'b0011: MTEMP2 = 4'b0010; // +2
           4'b0100: MTEMP2 = 4'b0010; // +2
           4'b0101: MTEMP2 = 4'b0100; // +3
           4'b0110: MTEMP2 = 4'b0100; // +3
           4'b0111: MTEMP2 = 4'b1000; // +4
           4'b1000: MTEMP2 = 4'b0111; // -4
           4'b1001: MTEMP2 = 4'b1011; // -3
           4'b1010: MTEMP2 = 4'b1011; // -3
           4'b1011: MTEMP2 = 4'b1101; // -2
           4'b1100: MTEMP2 = 4'b1101; // -2
           4'b1101: MTEMP2 = 4'b1110; // -1
           4'b1110: MTEMP2 = 4'b1110; // -1
           4'b1111: MTEMP2 = 4'b1010; // -0

	 endcase

      end

      {MBOOTH_P[i*4+3],
       MBOOTH_P[i*4+2],
       MBOOTH_P[i*4+1],
       MBOOTH_P[i*4+0]} = MTEMP2[3:0];

   end

   //  always @(/*AUTOnotSENSE*/
   //           __CEIopMADH_E_D2_R or __CEIopMAZH_E_D2_R or __DIV2HI or
   //           __MULTUSCYC or __MULTSHCYC or
   //           __DDATAH or __HI_R or __MCLA or __MCLASH)  begin

   //  always @(/*AUTOSENSE*/DDATAH or HI_R or MCLA or MCLASH)  begin
`define DMCLASH MCLASH
`define DCONST 1'b1
   always @(/*AUTOSENSE*/CEIopMADH_E_D2_R or CEIopMAZH_E_D2_R or DDATAH or DIV2HI or MCLA or MCLASH
	    or MULTSHCYC or MULTUSCYC)  begin

      case (1'b1)
	CEIopMADH_E_D2_R: HI_P = MCLA;
	CEIopMAZH_E_D2_R: HI_P = MCLA;
	DIV2HI:           HI_P = DDATAH;
	MULTUSCYC:        HI_P = MCLA;
	MULTSHCYC:        HI_P = `DMCLASH;
	default:          HI_P = `DCONST;
      endcase
   end
endmodule

// Local Variables:
// verilog-auto-read-includes:t
// End:
// New File: <sol_asense.v>
module dpi_test ();
   /* FIXME
   import "DPI-C" context task c_task (input  int i,
				       output int o);
      logic [7:0] 				  byte_array[1:20];
    */
endmodule // dpi_test
// New File: <sv_import.v>
module foo(input bit [3:0] def,
    input bit         ghi,
    input bit [1:0] jkl);
   
   task cba(input bit [3:0] a,
    input b,
	    c);
   endtask // cba
   task abc(input bit [3:0] def,
    input bit         ghi,
    input bit [1:0] jkl);
      
   endtask // abc
endmodule // foo
// New File: <task.v>
module t_autoinst_def_clk
   (/*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		a,			// To sub of sub.v
   input		b,			// To sub of sub.v
   input		clk,			// To sub of sub.v
   // End of automatics

    /*AUTOOUTPUT*/
    // Beginning of automatic outputs (from unused autoinst outputs)
    output		c			// From sub of sub.v
    // End of automatics
    );

   sub sub
      (/*AUTOINST*/
       // Outputs
       .c				(c),
       // Inputs
       .clk				(clk),
       .a				(a),
       .b				(b));

endmodule // sdc_wombat

module sub
   (input clk,

    input a,
    input b,

    output c
    );

   clocking cb
     @(posedge clk);
   endclocking

   default clocking cb;

endmodule
// New File: <t_autoinst_def_clk.v>
//Verilint 182 off // WARNING: Illegal statement for synthesis: $realtobits (in1)
//Verilint 311 off // WARNING: Converting real to unsigned: $realtobits (in1)
//Verilint 20 off  // WARNING: Assign statement may not be synthesizable: assign out7[i] = ...;
//Verilint 599 off // WARNING: This construct is not supported by Synopsys
//Verilint 433 off // WARNING: More than one top level module
//Verilint  71 off // WARNING: Case statement without default clause

module testmodule (/*AUTOARG*/
   // Outputs
   out1, out2, out3, out4, out5, out7, out8, outb2, outb3, outb4, outb6, outb7, outb8, outb9,
   outb10, outw1, outw2, outw3,
   // Inputs
   in1, in2, in3, in4, in5
   );


   function [2:0] ffs;
      input [2:0] in;
      ffs = in & 3'b010;
   endfunction

   task show;
      input [2:0] in;
      begin
	 $display ("Hi %x", in);
      end
   endtask

   input [2:0] in1,in2,in3,in4,in5;
   output [2:0] out1, out2,out3,out4,out5,out7,out8;
   output 	outb2,outb3,outb4,outb6,outb7,outb8,outb9,outb10;
   output [7:0] outw1,outw2,outw3;
   reg [2:0] 	memarry [0:2];

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg [2:0]		out1;
   reg [2:0]		out2;
   reg [2:0]		out3;
   reg [2:0]		out4;
   reg [2:0]		out5;
   reg [2:0]		out8;
   reg			outb2;
   reg			outb3;
   reg			outb4;
   reg			outb6;
   reg			outb7;
   reg [7:0]		outw1;
   reg [7:0]		outw2;
   reg [7:0]		outw3;
   // End of automatics

   wire 	outb8=1'b1, outb9=|{in1[0],in2[0]}, outb10=1'b0;

   always @(/*AUTOSENSE*/in1 or in2 or in3 or in4) begin
      :ignore_label
      out1 = $realtobits(in1);
      out2 = ffs(in1 | (in2) );
      out3 = ffs /*check*/ (in2);
      $display ("chk ", in1);
      show (in4);
      if (|in3) out4=1; else out4=0;
   end

   always @ (/*AUTOSENSE*/in1 or in2 or in3 or in5) begin
      casex ({in5[1:0], (3'b010==in2)})
	3'bx_1_0: out5=3'b000;
	3'bx_1_1: out5=3'b010;
	3'bx_0_x: out5=3'b100;
      endcase
      casex ({in3[in1]})
	1'bx: out5=3'b000;
      endcase
   end

   /*AUTO_CONSTANT (`temp) */

`define temp 3'b010
   always @(/*AUTOSENSE*/in3) begin
      outb6 = (in3 == `temp);
   end

   integer     i;
   reg [2:0]   out7;
   always @ (/*AUTOSENSE*/in1) begin
      for (i=0; i<3; i=i+1) begin
	 assign out7[i] = ~in1[i];
      end
   end

   always @ (/*AUTOSENSE*/in1 or in2 or in3) begin
      {outw1 [ffs(in1)], outw2 [ffs(in2)]} = 2'b10;
      {outw3[(|in1)?in2:in3], outb2} = 2'b10;
   end

   initial memarry[0] = in2;
   always @ (/*AUTOSENSE*/ /*memory or*/ in1) begin
      $display (memarry[in1]);
   end

   always @(/*AUTOSENSE*/in1 or in2)
     casex(in1[1:0]) // synopsys full_case parallel_case
       2'b01 :  out8 = 3'b001;
       2'b10 :  out8 = 3'b010;
       default
	 out8 = in2;
     endcase

   parameter READ  = 3'b111,
	       //WRITE = 3'b111,
	       CFG   = 3'b010;
   //supply1   one;

   always @(/*AUTOSENSE*/in1 or in2) begin
      outb7 = (in1==READ) || (in2==CFG);
   end

   always @(/*AUTOSENSE*/in1) begin
      if (|in1) $display("We're at %t\n",$time);
   end // case: default

`define shift_instr 5'b01011
   always @(/*AUTOSENSE*/in1 or in2 or in3 or in4 or in5 or outw1)
     /*AUTO_CONSTANT(`shift_instr)*/
     begin: label_no_sense
	casex (outw1) // synopsys full_case parallel_case
	  {`shift_instr,3'bxxx}:
	    outb3 = in3[0];
	  8'b00001x10: outb3 = in4[0];
	  8'b00110011:
	    if (in5[0])
	      outb3 = in1[0];
	    else
	      outb3 = in2[1];
	  default
	    outb3 = in4[0];
	endcase
     end

   parameter WIDLE		= 0;		// No Manual Write Burst

   always @ (/*AUTOSENSE*/in1 or in2 or in3 or in4) begin
      case(1'b1)
	in2[WIDLE]:
	  outb4 = in1[0];
	in3[in4]:
	  outb4 = in1[0];
	default:
	  outb4 = 1'bx;
      endcase
   end

endmodule

module darren_jones_2 (/*AUTOARG*/
   // Outputs
   next_WSTATE,
   // Inputs
   WSTATE
   );
   input [1:0] WSTATE;
   output [1:0] next_WSTATE;
   reg [1:0] 	next_WSTATE;
   parameter
     WIDLE		= 0,		// No Manual Write Burst
       WCB0		= 1;		// 1st of the 4 Manual Write Burst

   always @ (/*AUTOSENSE*/WSTATE) begin
      next_WSTATE = 2'b0;
      case (1'b1)
	WSTATE[WIDLE]:
	  next_WSTATE[1'b0] = 1'b1;
	WSTATE[WCB0]:
	  next_WSTATE[WCB0] = 1'b1;
      endcase
   end
endmodule

module darren_jones_3 (/*AUTOARG*/
   // Outputs
   var1,
   // Inputs
   state
   );
   input [2:1] state;
   output      var1;
   reg 	       var1;

   parameter
     IDLE = 1,
       CAS1 = 2;

   always @(/*AUTOSENSE*/state) begin
      case (1'b1)
	state[IDLE] : begin
	   var1 = 1'b1;
	end
	state[CAS1] : begin
	   var1 = 1'b1;
	end
	default : begin
	   var1 = 1'b1;
	end
      endcase
   end

   always @(/*AUTOSENSE*/add or lo or mc_32pff or mc_losel or slo or var1) begin
      case(mc_losel)
	6'b000001:  lo_mux = mc_32pff ? {add[39:0],lo[31:8]} :
			     {add[7:0],lo[63:8]};

	6'b010000:  lo_mux = lo;
        6'b100000:  lo_mux = var1 ? IDLE : slo;
      endcase
   end // always @ (...

endmodule
// New File: <testcases.v>
// $Id: tss_max32.v,v 1.6 1999/01/20 17:34:54 wsnyder Exp $
//============================================================================

`time_scale

module tss_max32 (/*AUTOARG*/
   // Outputs
   max,
   // Inputs
   a, b
   );

   //======================================================================
   // Inputs/Outputs
   //======================================================================

   input [31:0]	  a;		// Time a
   input [31:0]   b;		// Time b
   output [31:0]  max;		// MAX (a,b)

   //======================================================================
   // Automatic Wire/Register Declarations
   //======================================================================

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   // End of automatics

   //======================================================================
   // Comparison
   //======================================================================

   wire 	  alessb;	// a<b or carry
   //Verilint 110 off // WARNING: Incompatible width
   DW01_cmp2 #(31) cmp (.LT_LE(alessb), .GE_GT(unused_ok),
			.A(a[30:0]),
			.B(b[30:0]),
			.LEQ(1'b0), .TC(1'b0));
   //Verilint 110 on  // WARNING: Incompatible width

   // Note because a has more bits we MUST choose it if a[31:8]==b[31:8]!
   wire 	  sela = ((a[31] != b[31]) ^ alessb);
   wire [31:0] 	  max = (sela ? b : a);

endmodule
// New File: <tss_max32.v>
// Example module that doesn't instantiate
module appendix1 (/*AUTOARG*/
   // Outputs
   z,
   // Inputs
   i
   );
   input i;
   output z;

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   reg			z;
   // End of automatics

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics

   always @ (/*AUTOSENSE*/i) begin
      z = i;
   end
endmodule

// Example module that does instantiate
module appendix2 (/*AUTOARG*/
   // Outputs
   z,
   // Inputs
   i
   );

   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input		i;			// To apx10 of appendix1.v, ...
   // End of automatics

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output [11:10]	z;			// From apx10 of appendix1.v, ...
   // End of automatics

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   // End of automatics

   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   // End of automatics

   /*
    appendix1 AUTO_TEMPLATE (
    .z	(z[@]),
    );
    */

   appendix1 apx10 (/*AUTOINST*/
		    // Outputs
		    .z			(z[10]),		 // Templated
		    // Inputs
		    .i			(i));

   appendix1 apx11 (/*AUTOINST*/
		    // Outputs
		    .z			(z[11]),		 // Templated
		    // Inputs
		    .i			(i));

endmodule
// New File: <two_modules.v>

// 2001 Parameter Style
module v2k_inst_hicks (
		       output wire [7:0] relay,
		       output wire relay_f,
		       output wire swen_f0_n,
		       input [31:0] dcb,
		       input relay_ce,
		       input wire simReset,
		       input wire clock
		       );

   input [31:0] 	     dcb_n2k;

   // AUTOINST results  NOT OK
   v2k_inst_hicks v2k_inst_hicks (/*AUTOINST*/
				  // Outputs
				  .relay		(relay[7:0]),
				  .relay_f		(relay_f),
				  .swen_f0_n		(swen_f0_n),
				  // Inputs
				  .dcb			(dcb[31:0]),
				  .relay_ce		(relay_ce),
				  .simReset		(simReset),
				  .clock		(clock),
				  .dcb_n2k		(dcb_n2k[31:0]));

endmodule
// New File: <v2k_inst_hicks.v>

// 2001 Parameter Style
module v2k_localparam;

   localparam X = 10;
   parameter  Y = 10;

   reg 	      z;

   always @ (/*AS*/) begin
      z = X | Y;
   end

endmodule
// New File: <v2k_localparam.v>
module foo_bar (/*AUTOARG*/
   // Outputs
   result,
   // Inputs
   clk, rst, data, addr
   )

  input        clk;
   input       rst;
   input       signed [15:-15] data;
   input [11:0] 	       addr;
   output 	signed [15:0]  result;

   wire 	signed [15:-15] data;
   reg 		signed [15:0] 	result;

   always @ (/*AS*/rst) begin
      result = 32'sh22 | rst;
   end

endmodule // foo_bar
// New File: <v2k_signed_kundsen.v>
`ifdef __TEST_I

`else

 `define __TEST_I

 `ifdef TWO_STATE_LOGIC
typedef bit   logic_t;  // Use two-state logic
 `else
typedef logic logic_t;  // Use four-state logic
 `endif

typedef reg   ff_t;     // Default F/F type
typedef reg   lat_t;    // Default latch type

//----------------------------
// 24 bit wide pixel types
//
typedef union packed {
		      logic_t [23:0] bits;
		      struct packed {
				     logic_t [7:0] red;
				     logic_t [7:0] grn;
				     logic_t [7:0] blu;
				     } color;
		      } pixel24_t;

`endif
// New File: <v2k_typedef_yee_inc.v>

`include "v2k_typedef_yee_inc.v"
module v2k_typedef_yee_sub1
  (
   output pixel24_t sub1_out_pixel,
   input  pixel24_t sub1_in_pixel,
   input  logic_t   cp,
   input  logic_t   reset,
   output logic_t   sub1_to_sub2,
   output logic_t   sub1_to_sub2_and_top
   );


   pixel24_t pixel_ff;


   always_ff @(posedge cp) begin
      pixel_ff <= sub1_in_pixel;
   end

   assign sub1_out_pixel = pixel_ff;
   assign sub1_to_sub2 = '1;
   assign sub1_to_sub2_and_top = '1;

endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <v2k_typedef_yee_sub1.v>

`include "v2k_typedef_yee_inc.v"
module v2k_typedef_yee_sub2
  (/*AUTOARG*/
   // Outputs
   sub2_out_pixel, ready,
   // Inputs
   sub2_in_pixel, cp, reset, sub1_to_sub2, sub1_to_sub2_and_top, pixel_ff
   );

   output pixel24_t sub2_out_pixel,
	  output logic_t   ready,
	  input  pixel24_t sub2_in_pixel,
	  input  logic_t   cp,
	  input  logic_t   reset,
	  input  logic_t   sub1_to_sub2,
	  input  logic_t   sub1_to_sub2_and_top

	  pixel24_t pixel_ff;
   ff_t      sub1_to_sub2_ff;


   always_ff @(posedge cp) begin
      pixel_ff <= sub2_in_pixel;
      sub1_to_sub2_ff <= sub1_to_sub2;
   end

   assign sub2_out_pixel = pixel_ff;
   assign ready = sub1_to_sub2_ff;

endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <v2k_typedef_yee_sub2.v>

`include "v2k_typedef_yee_inc.v"
module v2k_typedef_yee
  (/*AUTOARG*/
   // Outputs
   sub2_out_pixel, ready, sub1_to_sub2_and_top,
   // Inputs
   sub1_in_pixel, reset, pixel_ff, cp
   );

   //-----------------------
   // Output definitions
   //
   output logic_t        sub1_to_sub2_and_top;   // Explicit output port

   /*AUTOOUTPUT*/
   // Beginning of automatic outputs (from unused autoinst outputs)
   output logic_t	ready;			// From itest_sub2 of v2k_typedef_yee_sub2.v
   output pixel24_t	sub2_out_pixel;		// From itest_sub2 of v2k_typedef_yee_sub2.v
   // End of automatics


   //-----------------------
   // Input definitions
   //
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input logic_t	cp;			// To itest_sub1 of v2k_typedef_yee_sub1.v, ...
   input pixel24_t	pixel_ff;		// To itest_sub2 of v2k_typedef_yee_sub2.v
   input logic_t	reset;			// To itest_sub1 of v2k_typedef_yee_sub1.v, ...
   input pixel24_t	sub1_in_pixel;		// To itest_sub1 of v2k_typedef_yee_sub1.v
   // End of automatics


   //-----------------------
   // Wire definitions
   //
   /*AUTOWIRE*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   pixel24_t		sub1_out_pixel;		// From itest_sub1 of v2k_typedef_yee_sub1.v
   logic_t		sub1_to_sub2;		// From itest_sub1 of v2k_typedef_yee_sub1.v
   // End of automatics



   //-----------------------
   // Module instantiations
   //
   v2k_typedef_yee_sub1 itest_sub1
     (/*AUTOINST*/
      // Outputs
      .sub1_out_pixel			(sub1_out_pixel),
      .sub1_to_sub2			(sub1_to_sub2),
      .sub1_to_sub2_and_top		(sub1_to_sub2_and_top),
      // Inputs
      .sub1_in_pixel			(sub1_in_pixel),
      .cp				(cp),
      .reset				(reset));

   /*v2k_typedef_yee_sub2 AUTO_TEMPLATE (
    .sub2_in_pixel (sub1_out_pixel),
    )
    */
   v2k_typedef_yee_sub2 itest_sub2
     (/*AUTOINST*/
      // Outputs
      .sub2_out_pixel			(sub2_out_pixel),
      .ready				(ready),
      // Inputs
      .sub2_in_pixel			(sub1_out_pixel),	 // Templated
      .cp				(cp),
      .reset				(reset),
      .sub1_to_sub2			(sub1_to_sub2),
      .sub1_to_sub2_and_top		(sub1_to_sub2_and_top),
      .pixel_ff				(pixel_ff));


endmodule

// Local Variables:
// verilog-typedef-regexp: "_t$"
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <v2k_typedef_yee.v>
module cdl_io (/*AUTOARG*/
   // Outputs
   topsig,
   // Inputs
   clk
   );

   input clk;
   output topsig;

   //Verilint 113 off // WARNING: in macro RSV_CDLBASE_RDWR, Multiple drivers to a flipflop

   reg 	  topsig;
`define TOPSIG	{topsig}

   always @ (posedge clk) begin
      `TOPSIG <= #0 1'b1;
   end

   task direct_write;
      input val;
      begin
	 `TOPSIG = val;
      end
   endtask
endmodule
// New File: <verilint_113.v>
module Sub ();
   input a;
endmodule

module t ();

   /* Sub AUTO_TEMPLATE(
       .unused (x));
    */

   Sub sub (/*AUTOINST*/
	    // Inputs
	    .a				(a));

endmodule

// Local Variables:
// verilog-warn-fatal: t
// End:
// New File: <warn_fatal.v>
`define FOO 1'b1
module foo(__a,b);

   input __a;
   output b;

   always @(/*AUTOSENSE*/__a or `FOO) begin
      b = __a ^ `FOO ;
   end

endmodule

// New File: <wasson.v>
always
   @(w or c_int)
  begin
     if ((w == 1'b0) && ((status_register[7]) == 1'b1))
       begin
	  write_protect <= `TRUE ;
       end
     if (w == 1'b1)
       begin
	  write_protect <= `FALSE ;
       end
  end
   
// New File: <xx.v>

covergroup CB;
   a: coverpoint tr.a
     {
      bins a0 = {0};
      bins a1 = {1};
      option.weight=0;
   }
   b: coverpoint tr.b {
          bins b0 = {0};
          bins b1 = {1};
          option.weight=0;
       }
           ab: cross a,b {
              bins a0b0 = binsof(a.a0) && binsof(b.b0);
              bins a1b0 = binsof(a.a1) && binsof(b.b0);
              bins b0   = binsof(b.b0);
           }
               endgroup // CB

// New File: <abc.sv>
module align_assign_expr (
input logic clk,
input logic rst_n,
input logic [3:0] i,
output logic o1,
output logic [3:0] o2,
output logic [3:0] o3
);

parameter WIDTH = 4;

logic [WIDTH-1:0] temp1 = 4'h0;
logic temp2 = 1'b0;
logic signed temp3 = '0;

always_ff @(posedge clk) begin : label
if (!rst_n) begin
o1 <= 1'b0;
o2[1:0] <= 2'b00;
o2[3] <= 1'b1;
o2[4] <= 1'b0;
o3 <= 4'h0;
end
else begin
o1 <= 1'b1;
o2[1:0] <= 2'b11;
o2[3] <= 1'b0;
o2[4] <= 1'b1;
o3 <= 4'h1;
end
end : label

assign o1 = &i;
assign o2[1:0] = i[1:0];
assign o2[WIDTH-1:2] = i[3:0];
assign o3[0] = i;
assign o3[1] = i;
assign o3[3:2] = {i, i};

endmodule : align_assign_expr

// Local Variables:
// verilog-align-assign-expr: t
// End:
// New File: <align_assign_expr.sv>
// Issue #1272

import bar_pkg1::*;
import foo_pkg1::*;

module alignment_test
import bar_pkg2::*;
import foo_pkg2::*;
     #(
       parameter DATA_WIDTH = 8,
       parameter ADDR_WIDTH = 4
       )
     (
      input wire                  clk,
      input wire                  res_n,
      input wire                  valid,
      input wire [DATA_WIDHT-1:0] data_in,
      input wire [ADDR_WIDTH-1:0] addr,
      output logic                accept
      );

     import bar_pkg3::*;
     import foo_pkg3::*;

     localparam LP_BAR_0 = 1;
     localparam LP_BAR_100 = 100;
     localparam logic [3:0]       LP_BAR_5 = 5;

endmodule

// Local Variables:
// verilog-auto-lineup: all
// End:
// New File: <align_bug1272.sv>
// Issue #585

module bug585;
parameter ABC = 1;
parameter integer ABC = 1;
endmodule

module bug585_ext;
parameter ABC = 1;
parameter integer ABCD = 1;
parameter logic [3:0] ABCDE = 4'hF;
localparam ABCDEF = 1;
localparam logic [15:0] ABCDEFG = 16'hFACE;
endmodule
// New File: <align_bug585.sv>
// Issue #960

module img_cnt(
               logic clk,
               logic rst,
               logic frame_vld,
               logic line_vld,
               logic data_vld
               );

   // Expect to restart the alignment from here
   int unsigned      frame_cnt = 0;
   int unsigned      line_cnt = 0;
   int unsigned      data_cnt = 0;

endmodule
// New File: <align_bug960.sv>
// Issue #435
module foo
(
                                // Global Interface Signals
 input clk, // core system clock
 input signal1, // Comment here which should be at "comment-column"
 input signal_long2, // Comment here which should be at "comment-column"
 input sig3,                    // Here, I've hit "tab" twice and this line is correct
 input s4, // Comment here which should be at "comment-column"
 input reset_L,         // module reset: global register
);
endmodule // foo


// Issue #898
module my_module
 ( input a, // sig a
    input bc, // sig bc
    input d // sig d
 );
//
endmodule


// Issue #922
module foo
(input logic clk, // Added with respect to #922 to test alignment
input my_pkg::user_type keep_type_way_left, //.  <==Keep-indent marker
output logic done); // Added with respect to #922 to test alignment

import my_pkg::*; // Added with respect to #922 to test alignment
logic [7:0] city_bus; // Added with respect to #922 to test alignment
var user_type keep_type_also_way_left; //.  <==Keep-indent marker
endmodule


// Issue #1157
module top
(
//Inputs
input CLK, //System Clock
input RST, //Active High Reset
input [3:0] CONTROL, //Control decoder.

//Outputs
output reg [7:0] LIVE_DATA, //1 byte data
output VALID //Valid Flag
);
endmodule


// Other tests (some of them not working @ June 2022)
module foo
(
// This comment should NOT get aligned
input logic a1,// comment
input logic [1:0] a2,//comment
input logic a3,
input logic a4,
input logic [1:0] a5,                  //comment
output logic z1,      //    comment
input logic [1:0] z2 /* comment */
   // This comment should NOT get aligned
);

localparam IDLE = 0,// '01
READ = 1,// '02
THINK = 2,// '04
SEND = 3,// '08
WAIT = 4,// '10
GET_ACK = 5,// '20
WAIT_BUS = 6;// '40

// This comment should NOT get aligned
logic s1;// comment
logic [1:0] s2;//comment
logic s3;
logic s4;
logic [1:0] s5;                  //comment
logic t1;      //    comment
logic [1:0] t2; /* comment */
logic [1:0] /* embedded comment should NOT get aligned */ t3;
   // This comment should NOT get aligned
 /* This multiline comment
      should NOT get aligned */
endmodule

// Local Variables:
// verilog-align-decl-expr-comments: nil
// verilog-align-typedef-words: ("user_type")
// End:

// New File: <align_decl_comments_nil.sv>
// Issue #435
module foo
(
                                // Global Interface Signals
 input clk, // core system clock
 input signal1, // Comment here which should be at "comment-column"
 input signal_long2, // Comment here which should be at "comment-column"
 input sig3,                    // Here, I've hit "tab" twice and this line is correct
 input s4, // Comment here which should be at "comment-column"
 input reset_L,         // module reset: global register
);
endmodule // foo


// Issue #898
module my_module
 ( input a, // sig a
    input bc, // sig bc
    input d // sig d
 );
//
endmodule


// Issue #922
module foo
(input logic clk, // Added with respect to #922 to test alignment
input my_pkg::user_type keep_type_way_left, //.  <==Keep-indent marker
output logic done); // Added with respect to #922 to test alignment

import my_pkg::*; // Added with respect to #922 to test alignment
logic [7:0] city_bus; // Added with respect to #922 to test alignment
var user_type keep_type_also_way_left; //.  <==Keep-indent marker
endmodule


// Issue #1157
module top
(
//Inputs
input CLK, //System Clock
input RST, //Active High Reset
input [3:0] CONTROL, //Control decoder.

//Outputs
output reg [7:0] LIVE_DATA, //1 byte data
output VALID //Valid Flag
);
endmodule


// Other tests (some of them not working @ June 2022)
module foo
(
// This comment should NOT get aligned
input logic a1,// comment
input logic [1:0] a2,//comment
input logic a3,
input logic a4,
input logic [1:0] a5,                  //comment
output logic z1,      //    comment
input logic [1:0] z2 /* comment */
   // This comment should NOT get aligned
);

localparam IDLE = 0,// '01
READ = 1,// '02
THINK = 2,// '04
SEND = 3,// '08
WAIT = 4,// '10
GET_ACK = 5,// '20
WAIT_BUS = 6;// '40

// This comment should NOT get aligned
logic s1;// comment
logic [1:0] s2;//comment
logic s3;
logic s4;
logic [1:0] s5;                  //comment
logic t1;      //    comment
logic [1:0] t2; /* comment */
logic [1:0] /* embedded comment should NOT get aligned */ t3;
   // This comment should NOT get aligned
 /* This multiline comment
      should NOT get aligned */
endmodule

// Local Variables:
// verilog-align-comment-distance: 0
// verilog-align-typedef-words: ("user_type")
// End:

// New File: <align_decl_comments_spacing_0.sv>
// Issue #435
module foo
(
                                // Global Interface Signals
 input clk, // core system clock
 input signal1, // Comment here which should be at "comment-column"
 input signal_long2, // Comment here which should be at "comment-column"
 input sig3,                    // Here, I've hit "tab" twice and this line is correct
 input s4, // Comment here which should be at "comment-column"
 input reset_L,         // module reset: global register
);
endmodule // foo


// Issue #898
module my_module
 ( input a, // sig a
    input bc, // sig bc
    input d // sig d
 );
//
endmodule


// Issue #922
module foo
(input logic clk, // Added with respect to #922 to test alignment
input my_pkg::user_type keep_type_way_left, //.  <==Keep-indent marker
output logic done); // Added with respect to #922 to test alignment

import my_pkg::*; // Added with respect to #922 to test alignment
logic [7:0] city_bus; // Added with respect to #922 to test alignment
var user_type keep_type_also_way_left; //.  <==Keep-indent marker
endmodule


// Issue #1157
module top
(
//Inputs
input CLK, //System Clock
input RST, //Active High Reset
input [3:0] CONTROL, //Control decoder.

//Outputs
output reg [7:0] LIVE_DATA, //1 byte data
output VALID //Valid Flag
);
endmodule


// Other tests (some of them not working @ June 2022)
module foo
(
// This comment should NOT get aligned
input logic a1,// comment
input logic [1:0] a2,//comment
input logic a3,
input logic a4,
input logic [1:0] a5,                  //comment
output logic z1,      //    comment
input logic [1:0] z2 /* comment */
   // This comment should NOT get aligned
);

localparam IDLE = 0,// '01
READ = 1,// '02
THINK = 2,// '04
SEND = 3,// '08
WAIT = 4,// '10
GET_ACK = 5,// '20
WAIT_BUS = 6;// '40

// This comment should NOT get aligned
logic s1;// comment
logic [1:0] s2;//comment
logic s3;
logic s4;
logic [1:0] s5;                  //comment
logic t1;      //    comment
logic [1:0] t2; /* comment */
logic [1:0] /* embedded comment should NOT get aligned */ t3;
   // This comment should NOT get aligned
 /* This multiline comment
      should NOT get aligned */
endmodule

// Local Variables:
// verilog-align-comment-distance: 10
// verilog-align-typedef-words: ("user_type")
// End:

// New File: <align_decl_comments_spacing_10.sv>
// Issue #435
module foo
(
                                // Global Interface Signals
 input clk, // core system clock
 input signal1, // Comment here which should be at "comment-column"
 input signal_long2, // Comment here which should be at "comment-column"
 input sig3,                    // Here, I've hit "tab" twice and this line is correct
 input s4, // Comment here which should be at "comment-column"
 input reset_L,         // module reset: global register
);
endmodule // foo


// Issue #898
module my_module
 ( input a, // sig a
    input bc, // sig bc
    input d // sig d
 );
//
endmodule


// Issue #922
module foo
(input logic clk, // Added with respect to #922 to test alignment
input my_pkg::user_type keep_type_way_left, //.  <==Keep-indent marker
output logic done); // Added with respect to #922 to test alignment

import my_pkg::*; // Added with respect to #922 to test alignment
logic [7:0] city_bus; // Added with respect to #922 to test alignment
var user_type keep_type_also_way_left; //.  <==Keep-indent marker
endmodule


// Issue #1157
module top
(
//Inputs
input CLK, //System Clock
input RST, //Active High Reset
input [3:0] CONTROL, //Control decoder.

//Outputs
output reg [7:0] LIVE_DATA, //1 byte data
output VALID //Valid Flag
);
endmodule


// Other tests (some of them not working @ June 2022)
module foo
(
// This comment should NOT get aligned
input logic a1,// comment
input logic [1:0] a2,//comment
input logic a3,
input logic a4,
input logic [1:0] a5,                  //comment
output logic z1,      //    comment
input logic [1:0] z2 /* comment */
   // This comment should NOT get aligned
);

localparam IDLE = 0,// '01
READ = 1,// '02
THINK = 2,// '04
SEND = 3,// '08
WAIT = 4,// '10
GET_ACK = 5,// '20
WAIT_BUS = 6;// '40

// This comment should NOT get aligned
logic s1;// comment
logic [1:0] s2;//comment
logic s3;
logic s4;
logic [1:0] s5;                  //comment
logic t1;      //    comment
logic [1:0] t2; /* comment */
logic [1:0] /* embedded comment should NOT get aligned */ t3;
   // This comment should NOT get aligned
 /* This multiline comment
      should NOT get aligned */
endmodule


// Local Variables:
// verilog-align-typedef-words: ("user_type")
// End:
// New File: <align_decl_comments.sv>
module align_param # (
parameter PARAM_1 = 0,
parameter PARAMETER_2 = 1
);

localparam LOCAL_1 = 1;
localparam LOCAL_LONG = 2;
localparam LOCAL_MULTILINE = 3,
LOCAL2 = 4,
LOCAL_LONG3 = 5;

endmodule
// New File: <align_param.sv>
module foo (input logic [7:0] in1,
input logic in2,
custom_type type1,
output logic out,
custom_type type2
);

logic [7:0] signal1;
logic signal2;
custom_type type3;

endmodule

// Local Variables:
// verilog-align-typedef-words: ("custom_type")
// End:
// New File: <align_ports_custom_type.sv>
class test;

virtual function void cmp_core
(
input bit [8:0] max_len,
input bit mv,
mv2, mv3,
mv3,
ref example_t algo_cfg,
ref bit [17:0] orig_img [],
ref bit [15:0] cmp_img [],
example_t algo_cfg,
example_e asdf,
example_if asdf_if,
example_vif asdf_if,
example_t[2] algo_cfg,
input bit recmp_en = 1'b0,
output bit [17:0] re_pixel_output_tmp
);

endfunction

endclass

// Local Variables:
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_\\(t\\|e\\|s\\|if\\|vif\\)\\>"
// End:
// New File: <align_verilog_typedef.sv>
typedef node logic;

module top
  #(parameter COLS = 4
    )
  ( input logic clk,
    input logic rstb,
    input logic [COLS-1:0] ival,
    input logic [COLS-1:0][1:0] idata_some_extra_sig_length,

    input logic [COLS-1:0][7:0] isig1,
    input logic [COLS-1:0][6:0] isig2,
    input logic [COLS-1:0][5:0] isig3,
    /*AUTOINPUT*/
    // Beginning of automatic inputs (from unused autoinst inputs)
    input		idata_some_extra_sig_lengt,// To isub1 of sub1.v, ...
    input		isig,			// To isub1 of sub1.v, ...
    // End of automatics

    output logic [COLS-1:0] oval,
    output logic [COLS-1:0][1:0] odata,
    output logic [COLS-1:0] s3_oval,
    output logic [COLS-1:0][1:0] s3_odata,

    /*AUTOOUTPUT*/
    // Beginning of automatic outputs (from unused autoinst outputs)
    output		s1_odat,		// From isub1 of sub1.v
    output		s3_odat		// From isub3 of sub3.v
    // End of automatics
    );


   logic [COLS-1:0][1:0]           s1_odata;               // From isub1 of sub1.v
   logic [COLS-1:0]           s1_oval;                // From isub1 of sub1.v
   /*AUTOWIRE*/
   
   /* sub2 AUTO_TEMPLATE(
    .idata_some_extra_sig_length ( s1_odata),
    .ival ( s1_oval),
    );*/
   sub2 isub2 (/*AUTOINST*/
	       // Outputs
	       .oval			(oval[COLS-1:0]),
	       .odata			(odata/*[COLS-1:0][1:0]*/),
	       // Inputs
	       .clk			(clk),
	       .rstb			(rstb),
	       .ival			( s1_oval),		 // Templated
	       .idata_some_extra_sig_length( s1_odata));		 // Templated

   genvar column;
   /* sub1 AUTO_TEMPLATE(
    .i.* ( @"vl-name"[column] @"(if (or(>(length vl-mbits)0) (>(length vl-bits)0)) (concat \\"/\\* \\" vl-mbits vl-bits \\" *\\/\\") )"),
    .o.* ( s1_@"vl-name"[column] @"(if (or(>(length vl-mbits)0) (>(length vl-bits)0)) (concat \\"/\\* \\" vl-mbits vl-bits \\" *\\/\\") )"),
    );*/
   /* sub3 AUTO_TEMPLATE(
    .i.* ( @"vl-name"[column] @"(if (or(>(length vl-mbits)0) (>(length vl-bits)0)) (concat \\"/\\* \\" vl-mbits vl-bits \\" *\\/\\") )"),
    .o.* ( s3_@"vl-name"[column] @"(if (or(>(length vl-mbits)0) (>(length vl-bits)0)) (concat \\"/\\* \\" vl-mbits vl-bits \\" *\\/\\") )"),
    );*/
   generate for(column=0;column<4;column++) begin : COLUMN
      sub1 isub1(/*AUTOINST*/
		 // Outputs
		 .oval			( s1_oval[column] ),	 // Templated
		 .odata			( s1_odata[column] /* [1:0] */), // Templated
		 // Inputs
		 .clk			(clk),
		 .rstb			(rstb),
		 .ival			( ival[column] ),	 // Templated
		 .idata_some_extra_sig_length( idata_some_extra_sig_length[column] /* [1:0] */), // Templated
		 .isig1			( isig1[column] /* [7:0] */), // Templated
		 .isig2			( isig2[column] /* [6:0] */), // Templated
		 .isig3			( isig3[column] /* [5:0] */)); // Templated
      sub3 isub3(/*AUTOINST*/
		 // Outputs
		 .oval			( s3_oval[column] ),	 // Templated
		 .odata			( s3_odata[column] /* [1:0] */), // Templated
		 // Inputs
		 .clk			(clk),
		 .rstb			(rstb),
		 .ival			( ival[column] ),	 // Templated
		 .idata_some_extra_sig_length( idata_some_extra_sig_length[column] /* [1:0] */)); // Templated
      
   end endgenerate

   
endmodule // top

module sub1
  ( input logic clk,
    input logic rstb,
    input logic ival,
    input logic [1:0] idata_some_extra_sig_length,
    input logic [7:0] isig1,
    input logic [6:0] isig2,
    input logic [5:0] isig3,
    output logic oval,
    output logic [1:0] odata
    );
endmodule // sub
module sub3
  ( input logic clk,
    input logic rstb,
    input logic ival,
    input logic [1:0] idata_some_extra_sig_length,
    output logic oval,
    output logic [1:0] odata
    );
endmodule // sub

module sub2
  #(parameter COLS = 4
    )
  ( input logic clk,
    input logic rstb,
    input logic [COLS-1:0] ival,
    input logic [COLS-1:0][1:0] idata_some_extra_sig_length,
    output logic [COLS-1:0] oval,
    output logic [COLS-1:0] [1:0] odata
    );
endmodule // sub
// Local Variables:
// verilog-typedef-regexp:"\\(^t_\\)\\|\\(^node$\\)\\|\\(_s$\\)\\|\\(_t$\\)"
// verilog-library-directories:("." )
// verilog-library-extensions:(".v" ".sv" ".h" ".vr" ".vm")
// End:
// New File: <autoinst_cmtparen_tennant.sv>
// Child module
//   ... with interface modport specified in module header

module autoinst_mplist_child
  (
   // --------------------------------------------------------------------------
   // Port Declarations
   // --------------------------------------------------------------------------
   
   input        clk,
   input        reset_n,
                
                // Yeah, let's have an interface
                autoinst_mplist_mbl_if.slave msg_req_if,
                autoinst_mplist_mbl_if.master msg_resp_if,
   
   // There are likely other signals present, too
   output logic msg_busy,
   output logic mem_rd_req,
   input        mem_rd_gnt
   );
   
   // Really snazzy RTL follows...
   
endmodule

/*
 Local Variables:
 verilog-typedef-regexp:"_t$"
 verilog-library-directories:(".")
 verilog-library-extensions:(".sv")
 verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_if\\>"
 End:
 */
// New File: <autoinst_mplist_child.sv>
// Example interface definition with at least one modport.

interface autoinst_mplist_mbl_if ();
   
   logic        sof;
   logic        eof;
   logic [31:0] data;
   logic        ready;
   logic        valid;
   
   modport master(
                  output data,
                  output sof,
                  output eof,
                  output valid,
                  input  ready
                  );
   
   modport slave(
                 input  data,
                 input  sof,
                 input  eof,
                 input  valid,
                 output ready
                 );
   
endinterface
// New File: <autoinst_mplist_mbl_if.sv>

// Parent module
//   Specifies modport for interfaces in header.
//   Has child modules which do the same.

module autoinst_mplist
  (
   // --------------------------------------------------------------------------
   // Port Declarations
   // --------------------------------------------------------------------------

   input                           clk,
   input                           reset_n,

   // Top-level interfaces
   mbl_if.master                   msg_resp_if

   );

   mbl_if                    msg_req_if;   // Some internal interface

  // --------------------------------------------------------------------------
  // Packages and Local Declarations
  // --------------------------------------------------------------------------

  /*AUTOLOGIC*/


  // --------------------------------------------------------------------------
  // Module Body
  // --------------------------------------------------------------------------

  // For the module instance below, interface ports should be
  // connected (per standard AUTOINST fashion) but *without*
  // explicitly specifying the modport.
  //
  // VCS (and likely other Synopsys tools) don't expect to see a
  // modport being "respecified" here.

  autoinst_mplist_child child
    (/*AUTOINST*/);


endmodule

/*
 Local Variables:
 verilog-typedef-regexp:"_t$"
 verilog-library-directories:(".")
 verilog-library-extensions:(".sv")
 verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_if\\>"
 End:
 */
// New File: <autoinst_mplist.sv>
module parent(/*AUTOARG*/);
  input  [31:0][7:0] data_in;
  output [4:0]       meta_out;
  output [4:0]       meta_out_no_rename;
  output [31:0][7:0] data_out;

  /*AUTOWIRE*/

  /* child1 AUTO_TEMPLATE (
    .data_out                 (child1_out[][]),
    .meta_out                 (child1_meta_out[]),
  ); */
  child1 U1 (
    /*AUTOINST*/
  )

  /* child2 AUTO_TEMPLATE (
    .data_in                 (child1_out[][]),
    .meta_in                 (child1_meta_out[]),
  ); */
  child2 U2 (
    /*AUTOINST*/
  )

endmodule

module child1(/*AUTOARG*/);
  input  [31:0][7:0] data_in;
  output [31:0][7:0] data_out;
  output [4:0]       meta_out;

endmodule

module child2(/*AUTOARG*/);
  input  [31:0][7:0] data_in;
  input  [4:0]       meta_in;
  output [31:0][7:0] data_out;
  output [4:0]       meta_out_no_rename;

endmodule

// Local Variables:
// verilog-auto-inst-vector:nil
// End:
// New File: <autoinst_multidim_rename.sv>
module top
  #(parameter COLS = 4
    )
  ( input logic clk,
    input logic rstb,
    input logic [COLS-1:0] ival,
    input logic [COLS-1:0][1:0] idata_some_extra_sig_length,
    output logic reg_assigned
    );

   // Request: AUTOLOGIC instead of AUTOWIRE
   // Request: AUTOLOGIC would substitute all 3 existing macros AUTOWIRE/AUTOREG/AUTOREGINPUT, one would use AUTOLOGIC instead of AUTOWIRE/AUTOREG or AUTOWIRE/AUTOREGINPUT.

   // Could use AU-TOLOGIC
   // Or        AU-TOWIRE(logic)  
   // And-or (setq verilog-auto-sv t)
   // And-or (setq verilog-auto-wire-type 'logic')

   /*AUTOOUTPUTEVERY*/
   // Beginning of automatic outputs (every signal)
   output logic		COLS;
   output logic		in1a;			// To isub2 of sub2.v, ...
   output logic		in1b;			// To isub1 of sub1.v
   output logic		in2b;			// To isub2 of sub2.v
   output logic		out1a;			// From isub1 of sub1.v
   output logic [1:0]	out1b;			// From isub1 of sub1.v
   output logic [1:0]	out2b;			// From isub2 of sub2.v
   // End of automatics

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   logic		reg_assigned;
   // End of automatics

   /*AUTOREGINPUT*/
   // Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
   logic		in1a;			// To isub2 of sub2.v, ...
   logic		in1b;			// To isub1 of sub1.v
   logic		in2b;			// To isub2 of sub2.v
   // End of automatics

   /*AUTOLOGIC*/
   // Beginning of automatic wires (for undeclared instantiated-module outputs)
   logic		out1a;			// From isub1 of sub1.v
   logic [1:0]		out1b;			// From isub1 of sub1.v
   logic [1:0]		out2b;			// From isub2 of sub2.v
   // End of automatics
   
   /* sub2 AUTO_TEMPLATE(
    .idata_some_extra_sig_length ( s1_odata),
    .ival ( s1_oval),
    );*/
   sub2 isub2 (/*AUTOINST*/
	       // Outputs
	       .out2b			(out2b[1:0]),
	       // Inputs
	       .in1a			(in1a),
	       .in2b			(in2b),
	       .out1a			(out1a));


   sub1 isub1(/*AUTOINST*/
	      // Outputs
	      .out1a			(out1a),
	      .out1b			(out1b[1:0]),
	      // Inputs
	      .in1a			(in1a),
	      .in1b			(in1b));

   always @(posedge clk) begin
      reg_assigned <= 1;
   end

endmodule

module sub1
  ( input logic in1a,
    input logic in1b,
    output logic out1a,
    output logic [1:0] out1b
    );
endmodule

module sub2
  ( input logic in1a,
    input logic in2b,
    input logic out1a,
    output logic [1:0] out2b
    );
endmodule

// Local Variables:
// verilog-auto-wire-type: "logic"
// End:
// New File: <autologic.sv>
module test_port0
(/*AUTOARG*/);
    output [31:0] d[2];
endmodule // test_port

module test_port1
(/*AUTOARG*/);
   input [31:0] dd[2];
endmodule // test_port1

module top()
/*AUTOWIRE*/

/*
   test_port0  AUTO_TEMPLATE
   (
   .d\(.*\)         ('{d_1\1[],d_2\1[]}),
   );
*/
test_port0
  u0(/*AUTOINST*/);

  /*
   test_port1  AUTO_TEMPLATE
   (
   .d\(.*\)         ('{d_1\1[],d_2\1[]}),
   );
*/

test_port1
  u1(/*AUTOINST*/);

endmodule // top
// New File: <autowire_apostrophe.sv>
module autowire_topv_two;
   output logic [1:0] foo2;
   output logic foo3;
   output logic [1:0] bar2;
   output logic bar3;
endmodule
// New File: <autowire_topv_two.sv>
package foo;

typedef logic [7:0] byte_t;
localparam byte_t ALL_ONES = 8'hFF;
localparam byte_t [3:0] ALL_ZEROS = 32'h0;

function foo3 (input int a);
$display(a);
endfunction

class A;
    byte_t foo2 = ALL_ONES;
endclass

endpackage

// Local Variables:
// verilog-indent-class-inside-pkg: nil
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <indent_class_pkg_nil.sv>
module test (
input logic a,
output logic b
);

for (genvar i=0; i<2; i++) begin : g_slice

submod
u_sub_0 #(
.RESET_POL (RESET_POL)
)(
.clk     (clk),
.reset_n (reset_n),
.d       (d[i]),
.q       (q_0[i])
);

submod
u_sub_1 #(
.RESET_POL (RESET_POL)
)(
.clk     (clk),
.reset_n (reset_n),
.d       (q_0[i]),
.q       (q[i])
);

end

endmodule


// Local Variables:
// indent-tabs-mode: nil
// End:
// New File: <indent_decl_1760.sv>
// Bug 1404

module test
  #(paramweter integer OPT = 1
     )
    (input logic y,z;
    );

    if (OPT = 1) begin
       always_comb begin
          y = 1'b1;
       end
end else begin
    always_comb begin
       y = 1'b0;
    end
end

    if (OPT = 1) begin
       assign z = 1'b1;
    end else begin
       assign z = 1'b0;
    end

endmodule // test
// New File: <indent_generate_bug1404.sv>
// Issue 949 - Indenting SVA assert final block broken
module tb;
begin
a0: assert final (data0 == 1) else
$error;
end
endmodule


// Now check for other types of immediate assertion
module tb;
always @(abcd) begin

// simple immediate assert statement
assert (xyz) a = b;

// deferred immediate cover statement w/ #0
if(x)
cover #0 (efg)
$display("covered");

// deferred immedate assume statement w/ final
assume final (abcd) else
$display();
end

endmodule
// New File: <indent_immediate_assertion.sv>
interface class ic;
// ...
endclass
// this should indent to left margin, but indented one stop to right
// New File: <indent_interface_class_bug1047.sv>
module foo (
input logic [7:0] in1,
input logic in2,
custom_type type1,
output logic out,
custom_type type2
);

logic [7:0] signal1;
logic signal2;
custom_type type3;

endmodule

// Local Variables:
// verilog-indent-lists: nil
// verilog-align-typedef-words: ("custom_type")
// End:
// New File: <indent_list_nil_align_ports_custom_type.sv>
module x;
initial begin
startc_c <= (valid && (state == THE_START));
end_c <= (valid && (state == THE_END));
valid_c <= (valid &&
(state != IDLE) &&
(state != SKIP_DATA));
end // initial begin
endmodule : x


module x;
initial begin
startc_c <= (valid && (state == THE_START));
end_c <= (
valid,
(state == THE_END)
);
valid_c <= { valid ,
(state != IDLE) ,
(state != SKIP_DATA)
};
end // initial begin
endmodule : x


module x;

initial begin
variable[i].value[0] = {signal3, signal2,
signal1, signal0};
end

endmodule: x


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_continued_line.sv>
// Issue #1774

module foo (
);

function int foo2 (input a,
output b);

if (some_long_name * some_other_long_name &&
something_here < something_else &&
something_here < something_else) begin
b = a;
end

if ((some_long_name * some_other_long_name &&
something_here < something_else &&
something_here < something_else)) begin
b = a;
end

if ((      some_long_name * some_other_long_name &&
something_here < something_else &&
something_here < something_else
)) begin
b = a;
end

if (
some_long_name * some_other_long_name &&
something_here < something_else &&
something_here < something_else
) begin
b = a;
end

if (some_long_name * some_other_long_name && something_here < something_else && something_here < something_else) begin
b = a;
end else if ( some_condition  ) begin
b = 0;
end
else begin
b = 1;
end

if (   some_long_name * some_other_long_name && something_here < something_else && something_here < something_else
) begin
b = a;
end else if (
some_condition
) begin
b = 0;
end
else begin
b = 1;
end

endfunction // foo2

endmodule // foo

// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_if.sv>
class test;

virtual function void func1 (
input bit [8:0] in1,
input bit in2,
ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1
);
out1 = {in1[7:0], in1[7:0]};
endfunction

virtual task task1 (
input bit [8:0] in1,
input bit in2,
ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1
);
out1 = {in1[7:0], in1[7:0]};
endtask

virtual task task1 (input bit [8:0] in1,
input bit in2,
ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1
);
out1 = {in1[7:0], in1[7:0]};
endtask

virtual task task1 (input bit [8:0] in1, input bit in2, ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1
);
out1 = {in1[7:0], in1[7:0]};
endtask

virtual task task1 (  input bit [8:0] in1, input bit in2, ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1
);
out1 = {in1[7:0], in1[7:0]};
endtask

virtual task task1 (  input bit [8:0] in1, input bit in2, ref example_t ref1,
ref bit [17:0] ref2 [],
ref bit [15:0] ref3 [],
example_t ex1,
input bit in3 = 1'b0,
output bit [17:0] out1);
out1 = {in1[7:0], in1[7:0]};
endtask

virtual task task1 (input bit [8:0] in1, input bit in2, ref example_t ref1);
out1 = {in1[7:0], in1[7:0]};
endtask

protected function void func2 (
input bit in,
output bit [3:0] out
);
out = |in;
endfunction

protected task task2 (
input bit in,
output bit [3:0] out
);
out = |in;
endtask

protected task task2 (input bit in,
output bit [3:0] out
);
out = |in;
endtask

protected task task2 ( input bit in,
output bit [3:0] out
);
out = |in;
endtask

protected task task2 ( input bit in,
output bit [3:0] out);
out = |in;
endtask

protected task task2 (input bit in, output bit [3:0] out);
out = |in;
endtask

static function void func3 (
input bit in,
output bit [3:0] out
);
out = |in;
endfunction

static task task3 (
input bit in,
output bit [3:0] out
);
out = |in;
endtask

static task task3 (input bit in,
output bit [3:0] out
);
out = |in;
endtask

static task task3 ( input bit in,
output bit [3:0] out
);
out = |in;
endtask

static task task3 ( input bit in,
output bit [3:0] out  );
out = |in;
endtask

static task task3 (input bit in, output bit [3:0] out  );
out = |in;
endtask

function void func4 (
input bit in,
output bit [3:0] out
);
out = &in;
endfunction

task task4 (
input bit in,
output bit [3:0] out
);
out = &in;
endtask

task task4 (input bit in,
output bit [3:0] out
);
out = &in;
endtask

task task4 ( input bit in,
output bit [3:0] out
);
out = &in;
endtask

task task4 ( input bit in,
output bit [3:0] out);
out = &in;
endtask

task task4 (input bit in, output bit [3:0] out);
out = &in;
endtask

endclass

// Local Variables:
// verilog-indent-lists: nil
// verilog-align-typedef-regexp: "\\<[a-zA-Z_][a-zA-Z_0-9]*_t\\>"
// End:
// New File: <indent_list_nil_methods.sv>
module param_port_list # (parameter integer PARAMETER_0 = 0, // Some comment
parameter integer PARAMETER_1 = 0,
parameter integer PARAMETER_2 = 1,
parameter integer PARAMETER_3 = 2,
parameter integer PARAMETER_4 = 3,
parameter integer PARAMETER_5 = 4, // Some other comment
parameter integer PARAM_6 = 5,
parameter integer PARAMETER7 = 6,
parameter integer PARAM_LONGER = 7,
parameter integer PARAM_EVEN_LONGER = 8, // Maybe more comments
parameter integer PARAMETER_10 = 9,
parameter integer PARAMETER_11 = 10,
parameter integer PARAMETER_12 = 11,
parameter integer PARAMETER_13 = 12)
(
input logic clk,
input logic rst_n,
input logic signal_1, // Random comment
input logic [PARAMETER_0-1:0] signal2,
input logic [PARAMETER_1-1:0] signal_longer_name,
input logic signal3,
output logic [PARAMETER_2-1:0] signal_other_name,
input logic [PARAMETER_3-1:0] another_signal,
output logic [PARAMETER_4-1:0] even_more_signals,
output logic output_signal,
output logic more_output_signals [PARAMETER_13],
input logic packed_array [PARAMETER_5][PARAMETER_6], // Another random comment
input logic [4:0][3:0] unpacked_array, // More comments
input logic [4:0] unpacked_array_2 // Last comment
);

typedef struct packed{
logic name1;
logic [1:0] name2;
logic name_3;
logic name_4;
} some_type_t;


endmodule



module param_port_list # (
parameter PARAMETER_0 = 0,
parameter integer PARAMETER_11 = 1,
parameter logic [3:0] PARAMETER_222 = 4'h0
)(
input logic clk,
input logic rst_n,
input logic [3:0] inputs, // Random comment
output logic [15:0] outputs
);


assign outputs[3:0] = inputs;

endmodule



class parameterized_class #(
type T1=int,
type T22=int,
type T333=int
) extends base_class;

T1 val;
T22 val2;
T333 val3;

endclass


class parameterized_class2 #(type T1=int,
type T22=int,
type T333=int) extends base_class;

T1 val;
T22 val2;
T333 val3;

endclass



// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_param_port_list.sv>
package foo;

class A;
rand int attribute1;
rand logic [3:0] attribute2;

constraint values_c {
attribute1 inside {[0:a]};
attribute2 inside {[0:15]};
}

function new (
int a,
logic [3:0] b
);
this.attribute1 = a;
this.attribute2 = b;
endfunction: new

task T1 (
input int a,
input logic [3:0] b
);
this.attribute1 = a;
this.attribute2 = b;
endtask: T1

endclass // A

endpackage // foo


// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_pkg_class.sv>
class foo;

int member;

function void foo2 (input asdf);
`uvm_info(get_name(), $sformatf("Here there is a long line with a variable to be printed, member=%0d",
member), UVM_MEDIUM)

`uvm_info(get_name(),
$sformatf("Here there is a long line with one argument per line, member=%0d",
member),
UVM_MEDIUM)
endfunction

endclass

// Local Variables:
// verilog-indent-lists: nil
// End:
// New File: <indent_list_nil_report.sv>
module typedef_enum_indent;

logic variable1;
logic signed [1:0] variable2;
logic variable3;
logic signed [1:0] variable4;

typedef enum logic [1:0] {STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum logic [1:0] {STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0] {
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0] {
STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum logic [1:0]
{STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0]
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0]
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
}
enum_t;

typedef enum {STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum {STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum {
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum {
STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum
{STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
}
enum_t;


endmodule


// Local Variables:
// verilog-indent-lists: nil
// End:

// New File: <indent_list_nil_typedef_enum.sv>
// Issue 1082

`define drive_agt(AGT_ID) \
  begin \
      some_agt_seq seq; \
      seq = some_agt_seq::type_id::create \
                 (.name({"some_agt_seq_",$sformatf("%0d", AGT_ID)}), \
                  .contxt(get_full_name())); \
      seq.start(env.adc_agt[AGT_ID].sqr_l1); \
  end


  `define foo(ARG) \
     begin \
       $display(\"Bar\"); \
       $display(\"Baz\"); \
     end


`define foo(ARG) \
     begin \
         $display(\"Bar\"); \
         $display(\"Baz\"); \
     end


// Indentation should also ignore multiline macros with trailing whitespaces
`define foo(ARG) \       
     begin \    
         $display(\"Bar\"); \       
         $display(\"Baz\"); \  
     end



// Some example module to check that the rest of indentation works fine
module ram_controller ();

ram_sp_sr_sw #(
.DATA_WIDTH(16),
.ADDR_WIDTH(8),
.RAM_DEPTH(256)
) ram (
clk,
address,
data,
cs,
we,
oe
)
;

endmodule



// Local Variables:
// verilog-indent-ignore-multiline-defines: t
// End:
// New File: <indent_macro_ignore_multiline.sv>
// Ignore indentation for outshine header lines that start with // *

// * Header1
   // * Header2
      // * Header3
         // ** SubHeader3


// * Header1
   // * Header2
// * Header3
          // ** SubHeader3

                             // * Header1
        // * Header2
                                                   // * Header3
             // ** SubHeader3


// Some example module to check that the rest of indentation works fine
module ram_controller ();

ram_sp_sr_sw #(
.DATA_WIDTH(16),
.ADDR_WIDTH(8),
.RAM_DEPTH(256)
) ram (
clk,
address,
data,
cs,
we,
oe
)
;

endmodule


// Local Variables:
// verilog-indent-ignore-regexp: "// \\*"
// End:
// New File: <indent_macro_ignore_regexp.sv>
// Issue #955

module test (input a, input b, output c);
        parameter s = 4;
        reg [s:0] myreg;
        always @(posedge a) begin
               if (b) begin
                  r <= {s{1'b0}};
               end // <-- this end will be improperly indented unless
            // 's' is replaced with e.g. 4
        end
end
   endmodule
// New File: <indent_replicate_bug955.sv>
module foo
// ...
function asdf;
// ...
endfunction
// ...
task asdf;
// ...
endtask
// ...
generate
// ...
endgenerate
// ...
covergroup
// ...
endgroup
// ...
property
// ...
endproperty
// ...
sequence
// ...
endsequence
// ...
endmodule
// ...

// ...
package foo;
// ...
class foo2;
// ...
endclass
// ...
endpackage
// ...

class foo3;
// ...
endclass
// ...

virtual class foo4;
// ...
endclass
// ...

interface class foo5;
// ...
endclass
// ...

class foo6;
// ...
class foo7;
// ...
endclass
// ...
endclass
// ...

program
// ...
endprogram
// ...
// New File: <indent_sexp.sv>
// Bug 636
module mymodule (input logic  reset_n,
                  input logic  clock,
                               streambus.sink sink,
                               streambus.source source,
                  output logic interrupt_pulse);

    logic [31:0]               test;
    streambus.source source;
endmodule
// New File: <indent_sv_interface_mp_bug636.sv>
typedef class burst_drv;
   
class burst_drv extends vmm_xactor;
   
   int EXECUTING;
   int OBSERVED;
   int SUB_OBSERVED;
   
   protected burst_drv_cfg cfg;
   local     burst_drv_cfg reset_cfg;
   protected burst_xn       rx_factory;
   local     burst_xn       reset_rx_factory;
   
   burst_xn_channel in_chan;
   burst_xn_channel obs_chan;
   
   burst_xn tr_main; // Current transaction in main()
   /* could have [extern] [virtual] [protected|local] task [static|automatic|] name (); */
   /* If extern -> then it is complete; other wise it is not complete */
   /* class could have:
    class c
       extern virtual static protected task t ();
    endclass
    task declaration could have
    task static t();
    endtask
    */
   class c;
   endclass // c
   function f();
      g;
   endfunction // f
   generate g;
      /* a lot of stuff */
   endgenerate
   task t();
      /**/
      /**/            
   endtask // t
   protected virtual task pv_t();
      /**/
   endtask // pv_t

   protected task p_t();
      /* ACK*/
   endtask // p_t
   virtual task v_t();
      /**/      
   endtask // v_t
   virtual protected task vp_t();
      /**/
   endtask // vp_t
   protected virtual task pv_t();
      /**/
   endtask // pv_t
   extern task e_t();
   extern virtual task ev_t();
   extern protected task ep_t();
   extern protected virtual task epv_t();
   extern protected virtual task main();
   generate g;
      /**/
   endgenerate
   
   extern virtual function void reconfigure(burst_drv_cfg cfg);
   extern virtual function void reset_xactor(reset_e rst_type = SOFT_RST);
   extern virtual function new (
				string     inst,
				int        stream_id,
				burst_drv_cfg   cfg 	       = null,
				burst_xn_channel in_chan       = null,
				burst_xn_channel obs_chan      = null,
				burst_xn         rx_factory    = null);
   virtual task start();
      super.start();
      this.tx_dma.start();
      this.rx_dma.start();
   endtask // start
   task start();
      super.start();
      this.tx_dma.start();
      this.rx_dma.start();
   endtask // start
   task static start();
      super.start();
      this.tx_dma.start();
      this.rx_dma.start();
   endtask // static
endclass : burst_drv

// New File: <indent_task_func_decl.sv>
module typedef_enum_indent;

logic variable1;
logic signed [1:0] variable2;
logic variable3;
logic signed [1:0] variable4;

typedef enum logic [1:0] {STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum logic [1:0] {STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0] {
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0] {
STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum logic [1:0]
{STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0]
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum logic [1:0]
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
}
enum_t;

typedef enum {STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum {STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum {
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum {
STATE_0,
STATE_1,
STATE_2,
STATE_3} enum_t;

typedef enum
{STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
} enum_t;

typedef enum
{
STATE_0,
STATE_1,
STATE_2,
STATE_3
}
enum_t;


endmodule

// New File: <indent_typedef_enum.sv>
package p;
typedef enum {a, b} type_t;
typedef enum {TASK, TASK2} type2_t;
typedef enum {Package, Class} type3_t;
endpackage
// New File: <indent_typedef.sv>
// issue 928 - 'pure virtual' items in classes break indentation
package p;
virtual class c extends parent;
pure virtual protected task t(type_t a, type_t b);
function f(string s="");
blah;
endfunction:f
endclass
endpackage
// New File: <indent_virtual_class.sv>
// Bug 888 -- verilog-label-be should remove existing comment labels before adding new ones
class c;
endclass // delete-me
// New File: <label_class.sv>
module foobar;
   
   sequence s_1;
      {txd, txdk} && (ppd == 3'b000) && !txe;
   endsequence // s_1
  
   sequence s_phy_transmitting;
      {rxd, rxdk} && 
	((ppd == 3'b000) || (ppd == 3'b001))
	&& rxv;
   endsequence // s_phy_transmitting
   
  
endmodule
// New File: <mac_test2.sv>
module param_port_list # (parameter integer PARAMETER_0 = 0, // Some comment
parameter integer PARAMETER_1 = 0,
parameter integer PARAMETER_2 = 1,
parameter integer PARAMETER_3 = 2,
parameter integer PARAMETER_4 = 3,
parameter integer PARAMETER_5 = 4, // Some other comment
parameter integer PARAM_6 = 5,
parameter integer PARAMETER7 = 6,
parameter integer PARAM_LONGER = 7,
parameter integer PARAM_EVEN_LONGER = 8, // Maybe more comments
parameter integer PARAMETER_10 = 9,
parameter integer PARAMETER_11 = 10,
parameter integer PARAMETER_12 = 11,
parameter integer PARAMETER_13 = 12)
(
input logic clk,
input logic rst_n,
input logic signal_1, // Random comment
input logic [PARAMETER_0-1:0] signal2,
input logic [PARAMETER_1-1:0] signal_longer_name,
input logic signal3,
output logic [PARAMETER_2-1:0] signal_other_name,
input logic [PARAMETER_3-1:0] another_signal,
output logic [PARAMETER_4-1:0] even_more_signals,
output logic output_signal,
output logic more_output_signals [PARAMETER_13],
input logic packed_array [PARAMETER_5][PARAMETER_6], // Another random comment
input logic [4:0][3:0] unpacked_array, // More comments
input logic [4:0] unpacked_array_2 // Last comment
);

typedef struct packed{
logic name1;
logic [1:0] name2;
logic name_3;
logic name_4;
} some_type_t;


endmodule



module param_port_list # (
parameter PARAMETER_0 = 0,
parameter integer PARAMETER_11 = 1,
parameter logic [3:0] PARAMETER_222 = 4'h0
)(
input logic clk,
input logic rst_n,
input logic [3:0] inputs, // Random comment
output logic [15:0] outputs
);


assign outputs[3:0] = inputs;

endmodule



class parameterized_class #(
type T1=int,
type T22=int,
type T333=int
) extends base_class;

T1 val;
T22 val2;
T333 val3;

endclass


class parameterized_class2 #(type T1=int,
type T22=int,
type T333=int) extends base_class;

T1 val;
T22 val2;
T333 val3;

endclass

// New File: <param_port_list.sv>
import avm_pkg::*;
import tinyalu_pkg::*;

module top;
   
   alu_operation req;
   string msg;
   integer i;
   
   initial begin
      $display ("---- A or B inside {0,8'hFF} ----");
      for (i = 1; i<=5; i++) begin
	 req = new();
	 assert( a() with {A} );
	 req.randomize() 
	   with 
	     {  
		A inside {0,8'hFF}; 
		B inside {0,8'hFF}; 
		};
$sformat (msg,"ALU Request: %s", req.convert2string());
avm_report_message("TOP",msg);
$display;
$display ("---- op  inside [add_op : mul_op] ----");

for (i = 1; i<=10; i++) begin
   req = new();
   assert(
	  req.randomize() with 
	  { 
	    op inside {[add_op : mul_op]};
	    }
	  );
$sformat (msg,"ALU Request: %s", req.convert2string());
avm_report_message("TOP",msg);
end // for (i = 1; i<=10; i++)
end // initial begin
      endmodule // req_reader


// New File: <set_membership.sv>
`define moo 1
class foo;
   extern function void bar();
endclass: foo

function void foo::bar();
   int variable;
endfunction: bar

`define moo 1
class fu;
   extern function void br();
endclass: fu

function void fu::br();
   int variable;
endfunction: br
// New File: <testcase.sv>

`include "vmm.sv"

class my_xactor extends vmm_xactor;
   typedef enum { AA, BB, CC } e_myenum;
   typedef class t_mydata;
   typedef class t_myclass;
   
   int          myint;
   int          myintarr[10];
   int          myintda[];
   int          myintaa_int[int];
   int          myintaa_str[string];
   
   string       mystr;
   string       mystrarr[10];
   string       mystrda[];
   string       mystraa_int[int];
   string       mystraa_str[string];

   e_myenum     myenum;
   e_myenum     myenumarr[10];
   e_myenum     myenumda[];
   e_myenum     myenumaa_int[int];
   e_myenum     myenumaa_str[string];

   t_mydata     mydata;
   t_mydata     mydataarr[10];
   t_mydata     mydatada[];
   t_mydata     mydataaa_int[int];
   t_mydata     mydataaa_str[string];

   t_myclass    myclass;
   t_myclass    myclassarr[10];
   t_myclass    myclassda[];
   t_myclass    myclassaa_int[int];
   t_myclass    myclassaa_str[string];

   vmm_channel  mych;
   vmm_channel  mycharr[10];
   vmm_channel  mychda[];
   vmm_channel  mychaa_int[int];
   vmm_channel  mychaa_str[string];

   vmm_xactor   myxact;
   vmm_xactor   myxactarr[10];
   vmm_xactor   myxactda[];
   vmm_xactor   myxactaa_int[int];
   vmm_xactor   myxactaa_str[string];

   vmm_subenv   mysub;
   vmm_subenv   mysubarr[10];
   vmm_subenv   mysubda[];
   vmm_subenv   mysubaa_int[int];
   vmm_subenv   mysubaa_str[string];

   vmm_scenario mysc;
   
   `vmm_data_member_begin(my_xactor)
     `vmm_data_member_scalar(myint, DO_ALL)
      `vmm_data_member_scalar_array(myintarr, DO_ALL )
      `vmm_data_member_da(myintda, DO_ALL )
      `vmm_data_member_scalar_aa_scalar(myintaa_int, DO_ALL )
      `vmm_data_member_scalar_aa_string(myintaa_str, DO_ALL )
      
      `vmm_data_member_string(mystr, DO_ALL)
      `vmm_data_member_string_array(mystrarr, DO_ALL)
      `vmm_data_member_string_da(mystrda, DO_ALL)
      `vmm_data_member_string_aa_scalar(mystraa_int, DO_ALL)
      `vmm_data_member_string_aa_string(mystraa_str, DO_ALL)
      
      `vmm_data_member_enum(myenum, DO_ALL)
      `vmm_data_member_enum_array(myenumarr, DO_ALL)
      `vmm_data_member_enum_da(myenumda, DO_ALL)
      `vmm_data_member_enum_aa_scalar(myenumaa_int, DO_ALL)
      `vmm_data_member_enum_aa_string(myenumaa_str, DO_ALL)
      
      `vmm_data_member_vmm_data(mydata, DO_ALL, DO_DEEP )
      `vmm_data_member_vmm_data_array(mydataarr, DO_ALL, DO_DEEP )
      `vmm_data_member_vmm_data_da(mydatada, DO_ALL, DO_DEEP )
      `vmm_data_member_vmm_data_aa_scalar(mydataaa_int, DO_ALL, DO_DEEP )
      `vmm_data_member_vmm_data_aa_string(mydataaa_str, DO_ALL, DO_DEEP )
      
      `vmm_data_member_handle(myclass, DO_ALL )
      `vmm_data_member_handle_array(myclassarr, DO_ALL )
      `vmm_data_member_handle_da(myclassda, DO_ALL )
      `vmm_data_member_handle_aa_scalar(myclassaa_int, DO_ALL )
      `vmm_data_member_handle_aa_string(myclassaa_str, DO_ALL )
   `vmm_data_member_end(my_xactor)
   
   `vmm_env_member_begin(my_xactor)
     `vmm_env_member_scalar(myint, DO_ALL)
      `vmm_env_member_scalar_array(myintarr, DO_ALL )
      `vmm_env_member_da(myintda, DO_ALL )
      `vmm_env_member_scalar_aa_scalar(myintaa_int, DO_ALL )
      `vmm_env_member_scalar_aa_string(myintaa_str, DO_ALL )

      `vmm_env_member_string(mystr, DO_ALL)
      `vmm_env_member_string_array(mystrarr, DO_ALL)
      `vmm_env_member_string_da(mystrda, DO_ALL)
      `vmm_env_member_string_aa_scalar(mystraa_int, DO_ALL)
      `vmm_env_member_string_aa_string(mystraa_str, DO_ALL)
      
      `vmm_env_member_enum(myenum, DO_ALL)
      `vmm_env_member_enum_array(myenumarr, DO_ALL)
      `vmm_env_member_enum_da(myenumda, DO_ALL)
      `vmm_env_member_enum_aa_scalar(myenumaa_int, DO_ALL)
      `vmm_env_member_enum_aa_string(myenumaa_str, DO_ALL)
      
      `vmm_env_member_vmm_data(mydata, DO_ALL )
      `vmm_env_member_vmm_data_array(mydataarr, DO_ALL )
      `vmm_env_member_vmm_data_da(mydatada, DO_ALL )
      `vmm_env_member_vmm_data_aa_scalar(mydataaa_int, DO_ALL )
      `vmm_env_member_vmm_data_aa_string(mydataaa_str, DO_ALL )
      
      `vmm_env_member_channel(mych, DO_ALL )
      `vmm_env_member_channel_array(mycharr, DO_ALL )
      `vmm_env_member_channel_da(mychda, DO_ALL )
      `vmm_env_member_channel_aa_scalar(mychaa_int, DO_ALL )
      `vmm_env_member_channel_aa_string(mychaa_str, DO_ALL )

      `vmm_env_member_xactor(myxact, DO_ALL )
      `vmm_env_member_xactor_array(myxactarr, DO_ALL )
      `vmm_env_member_xactor_da(myxactda, DO_ALL )
      `vmm_env_member_xactor_aa_scalar(myxactaa_int, DO_ALL )
      `vmm_env_member_xactor_aa_string(myxactaa_str, DO_ALL )    

      `vmm_env_member_subenv(mysub, DO_ALL )
      `vmm_env_member_subenv_array(mysubarr, DO_ALL )
      `vmm_env_member_subenv_da(mysubda, DO_ALL )
      `vmm_env_member_subenv_aa_scalar(mysubaa_int, DO_ALL )
      `vmm_env_member_subenv_aa_string(mysubaa_str, DO_ALL )      
   `vmm_env_member_end(my_xactor)
   
   `vmm_scenario_member_begin(my_xactor)
     `vmm_scenario_member_scalar(myint, DO_ALL)
      `vmm_scenario_member_scalar_array(myintarr, DO_ALL )
      `vmm_scenario_member_da(myintda, DO_ALL )
      `vmm_scenario_member_scalar_aa_scalar(myintaa_int, DO_ALL )
      `vmm_scenario_member_scalar_aa_string(myintaa_str, DO_ALL )

      `vmm_scenario_member_string(mystr, DO_ALL)
      `vmm_scenario_member_string_array(mystrarr, DO_ALL)
      `vmm_scenario_member_string_da(mystrda, DO_ALL)
      `vmm_scenario_member_string_aa_scalar(mystraa_int, DO_ALL)
      `vmm_scenario_member_string_aa_string(mystraa_str, DO_ALL)

      `vmm_scenario_member_enum(myenum, DO_ALL)
      `vmm_scenario_member_enum_array(myenumarr, DO_ALL)
      `vmm_scenario_member_enum_da(myenumda, DO_ALL)
      `vmm_scenario_member_enum_aa_scalar(myenumaa_int, DO_ALL)
      `vmm_scenario_member_enum_aa_string(myenumaa_str, DO_ALL)

      `vmm_scenario_member_vmm_data(mydata, DO_ALL, DO_DEEP )
      `vmm_scenario_member_vmm_data_array(mydataarr, DO_ALL, DO_DEEP )
      `vmm_scenario_member_vmm_data_da(mydatada, DO_ALL, DO_DEEP )
      `vmm_scenario_member_vmm_data_aa_scalar(mydataaa_int, DO_ALL, DO_DEEP )
      `vmm_scenario_member_vmm_data_aa_string(mydataaa_str, DO_ALL, DO_DEEP )

      `vmm_scenario_member_handle(myclass, DO_ALL )
      `vmm_scenario_member_handle_array(myclassarr, DO_ALL )
      `vmm_scenario_member_handle_da(myclassda, DO_ALL )
      `vmm_scenario_member_handle_aa_scalar(myclassaa_int, DO_ALL )
      `vmm_scenario_member_handle_aa_string(myclassaa_str, DO_ALL )

      `vmm_scenario_member_vmm_scenario(mysc, DO_ALL )
   `vmm_scenario_member_end(my_xactor)

   `vmm_subenv_member_begin(my_xactor)
     `vmm_subenv_member_scalar(myint, DO_ALL)
      `vmm_subenv_member_scalar_array(myintarr, DO_ALL )
      `vmm_subenv_member_da(myintda, DO_ALL )
      `vmm_subenv_member_scalar_aa_scalar(myintaa_int, DO_ALL )
      `vmm_subenv_member_scalar_aa_string(myintaa_str, DO_ALL )

      `vmm_subenv_member_string(mystr, DO_ALL)
      `vmm_subenv_member_string_array(mystrarr, DO_ALL)
      `vmm_subenv_member_string_da(mystrda, DO_ALL)
      `vmm_subenv_member_string_aa_scalar(mystraa_int, DO_ALL)
      `vmm_subenv_member_string_aa_string(mystraa_str, DO_ALL)

      `vmm_subenv_member_enum(myenum, DO_ALL)
      `vmm_subenv_member_enum_array(myenumarr, DO_ALL)
      `vmm_subenv_member_enum_da(myenumda, DO_ALL)
      `vmm_subenv_member_enum_aa_scalar(myenumaa_int, DO_ALL)
      `vmm_subenv_member_enum_aa_string(myenumaa_str, DO_ALL)

      `vmm_subenv_member_vmm_data(mydata, DO_ALL )
      `vmm_subenv_member_vmm_data_array(mydataarr, DO_ALL )
      `vmm_subenv_member_vmm_data_da(mydatada, DO_ALL )
      `vmm_subenv_member_vmm_data_aa_scalar(mydataaa_int, DO_ALL )
      `vmm_subenv_member_vmm_data_aa_string(mydataaa_str, DO_ALL )

      `vmm_subenv_member_channel(mych, DO_ALL )
      `vmm_subenv_member_channel_array(mycharr, DO_ALL )
      `vmm_subenv_member_channel_da(mychda, DO_ALL )
      `vmm_subenv_member_channel_aa_scalar(mychaa_int, DO_ALL )
      `vmm_subenv_member_channel_aa_string(mychaa_str, DO_ALL )

      `vmm_subenv_member_xactor(myxact, DO_ALL )
      `vmm_subenv_member_xactor_array(myxactarr, DO_ALL )
      `vmm_subenv_member_xactor_da(myxactda, DO_ALL )
      `vmm_subenv_member_xactor_aa_scalar(myxactaa_int, DO_ALL )
      `vmm_subenv_member_xactor_aa_string(myxactaa_str, DO_ALL )    

      `vmm_subenv_member_subenv(mysub, DO_ALL )
      `vmm_subenv_member_subenv_array(mysubarr, DO_ALL )
      `vmm_subenv_member_subenv_da(mysubda, DO_ALL )
      `vmm_subenv_member_subenv_aa_scalar(mysubaa_int, DO_ALL )
      `vmm_subenv_member_subenv_aa_string(mysubaa_str, DO_ALL )    
   `vmm_subenv_member_end(my_xactor)

   `vmm_xactor_member_begin(my_xactor)
     `vmm_xactor_member_scalar(myint, DO_ALL)
      `vmm_xactor_member_scalar_array(myintarr, DO_ALL )
      `vmm_xactor_member_da(myintda, DO_ALL )
      `vmm_xactor_member_scalar_aa_scalar(myintaa_int, DO_ALL )
      `vmm_xactor_member_scalar_aa_string(myintaa_str, DO_ALL )

      `vmm_xactor_member_string(mystr, DO_ALL)
      `vmm_xactor_member_string_array(mystrarr, DO_ALL)
      `vmm_xactor_member_string_da(mystrda, DO_ALL)
      `vmm_xactor_member_string_aa_scalar(mystraa_int, DO_ALL)
      `vmm_xactor_member_string_aa_string(mystraa_str, DO_ALL)

      `vmm_xactor_member_enum(myenum, DO_ALL)
      `vmm_xactor_member_enum_array(myenumarr, DO_ALL)
      `vmm_xactor_member_enum_da(myenumda, DO_ALL)
      `vmm_xactor_member_enum_aa_scalar(myenumaa_int, DO_ALL)
      `vmm_xactor_member_enum_aa_string(myenumaa_str, DO_ALL)

      `vmm_xactor_member_vmm_data(mydata, DO_ALL )
      `vmm_xactor_member_vmm_data_array(mydataarr, DO_ALL )
      `vmm_xactor_member_vmm_data_da(mydatada, DO_ALL )
      `vmm_xactor_member_vmm_data_aa_scalar(mydataaa_int, DO_ALL )
      `vmm_xactor_member_vmm_data_aa_string(mydataaa_str, DO_ALL )

      `vmm_xactor_member_channel(mych, DO_ALL )
      `vmm_xactor_member_channel_array(mycharr, DO_ALL )
      `vmm_xactor_member_channel_da(mychda, DO_ALL )
      `vmm_xactor_member_channel_aa_scalar(mychaa_int, DO_ALL )
      `vmm_xactor_member_channel_aa_string(mychaa_str, DO_ALL )

      `vmm_xactor_member_xactor(myxact, DO_ALL )
      `vmm_xactor_member_xactor_array(myxactarr, DO_ALL )
      `vmm_xactor_member_xactor_da(myxactda, DO_ALL )
      `vmm_xactor_member_xactor_aa_scalar(myxactaa_int, DO_ALL )
      `vmm_xactor_member_xactor_aa_string(myxactaa_str, DO_ALL )    
   `vmm_xactor_member_end(my_xactor)
   
endclass: my_xactor


// Local Variables:
// verilog-align-typedef-regexp: "\\<\\(e\\|t\\|vmm\\)_[a-zA-Z_][a-zA-Z_0-9]*\\>"
// End:

// New File: <vmm_regressions.sv>
