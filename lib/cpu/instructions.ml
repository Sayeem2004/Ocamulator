open UInt8
open UInt16
open Cpu

module Instructions = struct
    type _ memory_mode =
    | Accumulator : uint8 memory_mode
    | Absolute : uint16 -> uint16 memory_mode
    | AbsoluteX : uint16 -> uint16 memory_mode
    | AbsoluteY : uint16 -> uint16 memory_mode
    | Immediate : uint8 -> uint8 memory_mode
    | Indirect : uint16 -> uint16 memory_mode
    | XIndirect : uint8 -> uint16 memory_mode
    | IndirectY : uint8 -> uint16 memory_mode
    | Relative : uint8 -> uint16 memory_mode
    | Zeropage : uint8 -> uint8 memory_mode
    | ZeropageX : uint8 -> uint16 memory_mode
    | ZeropageY : uint8 -> uint16 memory_mode

    let decode_contents (cpu : CPU.t) (type a) (mode : a memory_mode) : uint8 =
        match mode with
        | Accumulator -> cpu.accumulator
        | Absolute abs_addr -> CPU.fetch_ui8 cpu abs_addr
        | AbsoluteX abs_addr_x ->
            abs_addr_x +++ !^(cpu.register_X) +++ ?^(cpu.flags.carr_bit)
            |> CPU.fetch_ui8 cpu
        | AbsoluteY abs_addr_y ->
            abs_addr_y +++ !^(cpu.register_Y) +++ ?^(cpu.flags.carr_bit)
            |> CPU.fetch_ui8 cpu
        | Immediate b -> b
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr |> CPU.fetch_ui8 cpu
        | XIndirect x_ind_addr -> !^ x_ind_addr +++ !^ (cpu.register_X) |> CPU.fetch_ui16 cpu |> CPU.fetch_ui8 cpu
        | IndirectY ind_addr_y -> CPU.fetch_ui16 cpu !^ ind_addr_y |> CPU.fetch_ui8 cpu |> ( ++ ) (cpu.register_Y ++ ?. (cpu.flags.carr_bit))
        | Relative b -> !^ b +++ cpu.program_counter |> CPU.fetch_ui8 cpu
        | Zeropage zero_addr ->  CPU.fetch_ui8 cpu !^ zero_addr
        | ZeropageX zero_addr_x -> !^ (zero_addr_x ++ cpu.register_X) |> CPU.fetch_ui8 cpu
        | ZeropageY zero_addr_y -> !^ (zero_addr_y ++ cpu.register_Y) |> CPU.fetch_ui8 cpu

    let decode_address (cpu : CPU.t) (type a) (mode : a memory_mode) : uint16 =
        match mode with
        | Absolute abs_addr -> abs_addr
        | AbsoluteX abs_addr_x ->
            abs_addr_x +++ !^(cpu.register_X) +++ ?^(cpu.flags.carr_bit)
        | AbsoluteY abs_addr_y ->
            abs_addr_y +++ !^(cpu.register_Y) +++ ?^(cpu.flags.carr_bit)
        | Indirect ind_addr -> CPU.fetch_ui16 cpu ind_addr
        | XIndirect x_ind_addr -> !^ x_ind_addr +++ !^ (cpu.register_X) |> CPU.fetch_ui16 cpu
        | IndirectY ind_addr_y -> CPU.fetch_ui16 cpu !^ ind_addr_y
        | Relative b -> !^ b +++ cpu.program_counter
        | Zeropage zero_addr ->  !^ zero_addr
        | ZeropageX zero_addr_x -> !^ (zero_addr_x ++ cpu.register_X)
        | ZeropageY zero_addr_y -> !^ (zero_addr_y ++ cpu.register_Y)
        | _ -> raise (Failure "Memory mode incompatible with decode address")

    let detect_overflow (op_1 : uint8) (op_2 : uint8) (res : uint8) : bool =
        (?- op_1 && ?- op_2 && not ?- res) || (not ?- op_1 && not ?- op_2 && ?- res)

    let adc_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let operand = decode_contents cpu mode in
        let summed_acc = cpu.accumulator ++ operand ++ ?. (cpu.flags.carr_bit) in
        let overflow = detect_overflow operand cpu.accumulator summed_acc in
        let neg_bit = ?- summed_acc in
        let zero_bit = ?* summed_acc in
        {cpu with accumulator = summed_acc;
        flags = {
            cpu.flags with zero = zero_bit;
            negative = neg_bit;
            carr_bit = overflow;
            overflow = overflow;
        }}

    let and_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let operand = decode_contents cpu mode in
        let modif_acc = cpu.accumulator &&. operand in
        let zero_bit = ?* modif_acc in
        let neg_bit = ?- modif_acc in
        {cpu with accumulator = modif_acc;
        flags = {
            cpu.flags with zero = zero_bit;
            negative = neg_bit;
        }}

    let asl_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        match mode with
        | Accumulator -> 
            let shifted_acc = cpu.accumulator << 1 in 
            let carry_bit = ?- (cpu.accumulator) in
            let neg_bit = ?- shifted_acc in
            let zero_bit = ?* shifted_acc in {
            cpu with accumulator = shifted_acc; 
            flags = {
                cpu.flags with
                zero = zero_bit;
                carr_bit = carry_bit;
                negative = neg_bit;
            }
        }
        | addr_mode -> 
            let operand_addr = decode_address cpu addr_mode in
            let operand_contents = decode_contents cpu addr_mode in
            let shifted_contents = operand_contents << 1 in
            let carry_bit = ?- operand_contents in
            let neg_bit = ?- shifted_contents in
            let zero_bit = ?* shifted_contents in
            CPU.write_ui8 cpu operand_addr shifted_contents; 
            {
                cpu with flags = {
                    cpu.flags with zero = zero_bit;
                    carr_bit = carry_bit;
                    negative = neg_bit;
                }
            }
    
    let bcc_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if not cpu.flags.carr_bit then let branch_addr = decode_address cpu mode 
        in {cpu with program_counter = branch_addr} else cpu

     let bcs_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if cpu.flags.carr_bit then let branch_addr = decode_address cpu mode in 
        {cpu with program_counter = branch_addr} else cpu

    let beq_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if cpu.flags.zero then let branch_addr = decode_address cpu mode in 
        {cpu with program_counter = branch_addr} else cpu

    let bit_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let zero_bit = ?* (mem_contents &&. cpu.accumulator) in
        let bit_6 = ?- (mem_contents << 1) in
        let bit_7 = ?- mem_contents in
        {cpu with flags = {
            cpu.flags with zero = zero_bit;
            overflow = bit_6;
            negative = bit_7;
        }}

    let bmi_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if cpu.flags.negative then let branch_addr = decode_address cpu mode in
        {cpu with program_counter = branch_addr} else cpu

    let bne_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if not cpu.flags.zero then let branch_addr = decode_address cpu mode in 
        {cpu with program_counter = branch_addr} else cpu
    
    let bpl_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if not (cpu.flags.negative) then let branch_addr = decode_address cpu mode in
        {cpu with program_counter = branch_addr} else cpu

    let brk_op (cpu : CPU.t) : CPU.t =
        let cpu_pushed_pc = CPU.push_stack_u16 cpu cpu.program_counter in
        let uint8_flags = CPU.flags_ui8 cpu in
        let cpu_pushed_flags = CPU.push_stack_u8 cpu_pushed_pc uint8_flags in
        let interrupt_vector = CPU.fetch_ui16 cpu_pushed_flags ~^ 0xFFFE in
        {cpu_pushed_flags with program_counter = interrupt_vector;
        flags = {
            cpu_pushed_flags.flags with break = true;
        }}

    let bvc_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if not cpu.flags.overflow then let branch_addr = decode_address cpu mode in
        {cpu with program_counter = branch_addr} else cpu

    let bvs_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        if cpu.flags.overflow then let branch_addr = decode_address cpu mode in
        {cpu with program_counter = branch_addr} else cpu

    let clc_op (cpu : CPU.t) : CPU.t =
        {cpu with flags = {
            cpu.flags with zero = false;
        }}
    
    let cld_op (cpu : CPU.t) : CPU.t =
        {cpu with flags = {
            cpu.flags with decimal = false;
        }}
    
    let cli_op (cpu : CPU.t) : CPU.t =
        {cpu with flags = {
            cpu.flags with interrupt = false;
        }}

    let clv_op (cpu : CPU.t) : CPU.t =
        {cpu with flags = {
            cpu.flags with overflow = false;
        }}
    
    let cmp_op (cpu : CPU.t) (type a) (mode: a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let cmp_mem_acc = cpu.accumulator -- mem_contents in
        let carry_flag = cmp_mem_acc <?> ~. 0 > 0 in
        let zero_flag = ?* cmp_mem_acc in
        let neg_flag = ?- cmp_mem_acc in
        {cpu with flags = {
            cpu.flags with carr_bit = carry_flag;
            zero = zero_flag;
            negative = neg_flag;    
        }}

    let cpx_op (cpu : CPU.t) (type a) (mode: a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let cmp_mem_x = cpu.register_X -- mem_contents in
        let carry_flag = cmp_mem_x <?> ~. 0 > 0 in
        let zero_flag = ?* cmp_mem_x in
        let neg_flag = ?- cmp_mem_x in
        {cpu with flags = {
            cpu.flags with carr_bit = carry_flag;
            zero = zero_flag;
            negative = neg_flag;    
        }}

    let cpy_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let cmp_mem_x = cpu.register_Y -- mem_contents in
        let carry_flag = cmp_mem_x <?> ~. 0 > 0 in
        let zero_flag = ?* cmp_mem_x in
        let neg_flag = ?- cmp_mem_x in
        {cpu with flags = {
            cpu.flags with carr_bit = carry_flag;
            zero = zero_flag;
            negative = neg_flag;    
        }}
    
    let dec_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let mem_addr = decode_address cpu mode in
        let dec_contents = mem_contents -- ~. 0x01 in
        let zero_flag = ?* dec_contents in
        let neg_flag = ?- dec_contents in
        CPU.write_ui8 cpu mem_addr dec_contents;
        {cpu with flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}

    let dex_op (cpu : CPU.t) : CPU.t =
        let dec_x = cpu.register_X -- ~. 0x01 in
        let zero_flag = ?* dec_x in
        let neg_flag = ?- dec_x in
        {cpu with register_X = dec_x; 
        flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}

    let dey_op (cpu : CPU.t) : CPU.t =
        let dec_y = cpu.register_Y -- ~. 0x01 in
        let zero_flag = ?* dec_y in
        let neg_flag = ?- dec_y in
        {cpu with register_Y = dec_y; 
        flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}

    let eor_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let operand = decode_contents cpu mode in
        let xor_acc = cpu.accumulator ||. operand in
        let zero_bit = ?* xor_acc in
        let neg_bit = ?- xor_acc in
        {cpu with accumulator = xor_acc;
        flags = {
            cpu.flags with zero = zero_bit;
            negative = neg_bit;
        }}

    let inc_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let mem_contents = decode_contents cpu mode in
        let mem_addr = decode_address cpu mode in
        let inc_contents = mem_contents ++ ~. 0x01 in
        let zero_flag = ?* inc_contents in
        let neg_flag = ?- inc_contents in
        CPU.write_ui8 cpu mem_addr inc_contents;
        {cpu with flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}

    let inx_op (cpu : CPU.t) : CPU.t =
        let inc_x = cpu.register_X ++ ~. 0x01 in
        let zero_flag = ?* inc_x in
        let neg_flag = ?- inc_x in
        {cpu with register_X = inc_x; 
        flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}
    
    let iny_op (cpu : CPU.t) : CPU.t =
        let inc_y = cpu.register_Y ++ ~. 0x01 in
        let zero_flag = ?* inc_y in
        let neg_flag = ?- inc_y in
        {cpu with register_Y = inc_y; 
        flags = {
            cpu.flags with zero = zero_flag;
            negative = neg_flag;
        }}

    let jmp_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let jmp_addr = decode_address cpu mode in
        {cpu with program_counter = jmp_addr}
    
    let jsr_op (cpu : CPU.t) (type a) (mode : a memory_mode) : CPU.t =
        let return_point = cpu.program_counter --- ~^ 0x0001 in
        let pushed_cpu = CPU.push_stack_u16 cpu return_point in
        let jmp_addr = decode_address pushed_cpu mode in
        {pushed_cpu with program_counter = jmp_addr} 
end
