```fortran
! 3D Chess Game in Fortran with AI Players
! Uses an 8x8x8 board with extended chess rules

module chess_types
    implicit none
    
    ! Piece types
    integer, parameter :: EMPTY = 0
    integer, parameter :: PAWN = 1
    integer, parameter :: KNIGHT = 2
    integer, parameter :: BISHOP = 3
    integer, parameter :: ROOK = 4
    integer, parameter :: QUEEN = 5
    integer, parameter :: KING = 6
    
    ! Player colors
    integer, parameter :: WHITE = 1
    integer, parameter :: BLACK = 2
    
    ! Board dimensions
    integer, parameter :: BOARD_SIZE = 8
    
    type :: Piece
        integer :: type
        integer :: color
        logical :: has_moved = .false.
    end type Piece
    
    type :: Move
        integer :: x1, y1, z1
        integer :: x2, y2, z2
        type(Piece) :: captured
    end type Move
    
    type :: GameState
        type(Piece) :: board(BOARD_SIZE, BOARD_SIZE, BOARD_SIZE)
        integer :: current_player = WHITE
        integer :: move_count = 0
        type(Move), allocatable :: move_history(:)
    end type GameState
    
end module chess_types

module chess_logic
    use chess_types
    implicit none
    
contains
    function is_valid_move(game, x1, y1, z1, x2, y2, z2) result(valid)
        type(GameState), intent(in) :: game
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        type(Piece) :: piece, target
        
        valid = .false.
        
        ! Check bounds
        if (x1 < 1 .or. x1 > BOARD_SIZE .or. y1 < 1 .or. y1 > BOARD_SIZE .or. z1 < 1 .or. z1 > BOARD_SIZE) return
        if (x2 < 1 .or. x2 > BOARD_SIZE .or. y2 < 1 .or. y2 > BOARD_SIZE .or. z2 < 1 .or. z2 > BOARD_SIZE) return
        
        ! Check if source has a piece
        piece = game%board(x1, y1, z1)
        if (piece%type == EMPTY) return
        
        ! Check if target is not own piece
        target = game%board(x2, y2, z2)
        if (target%color == piece%color .and. target%type /= EMPTY) return
        
        ! Check piece-specific moves
        select case (piece%type)
        case (PAWN)
            valid = is_valid_pawn_move(game, x1, y1, z1, x2, y2, z2, piece%color)
        case (KNIGHT)
            valid = is_valid_knight_move(x1, y1, z1, x2, y2, z2)
        case (BISHOP)
            valid = is_valid_bishop_move(game, x1, y1, z1, x2, y2, z2)
        case (ROOK)
            valid = is_valid_rook_move(game, x1, y1, z1, x2, y2, z2)
        case (QUEEN)
            valid = is_valid_queen_move(game, x1, y1, z1, x2, y2, z2)
        case (KING)
            valid = is_valid_king_move(x1, y1, z1, x2, y2, z2)
        end select
        
    end function is_valid_move
    
    function is_valid_pawn_move(game, x1, y1, z1, x2, y2, z2, color) result(valid)
        type(GameState), intent(in) :: game
        integer, intent(in) :: x1, y1, z1, x2, y2, z2, color
        logical :: valid
        integer :: direction
        
        valid = .false.
        direction = merge(1, -1, color == WHITE)
        
        ! Forward move
        if (x1 == x2 .and. y1 == y2) then
            if (z2 == z1 + direction .and. game%board(x2, y2, z2)%type == EMPTY) then
                valid = .true.
            ! Double move from starting position
            else if (z2 == z1 + 2*direction .and. game%board(x2, y2, z2)%type == EMPTY .and. &
                     game%board(x2, y2, z1 + direction)%type == EMPTY) then
                if ((color == WHITE .and. z1 == 2) .or. (color == BLACK .and. z1 == 7)) then
                    valid = .true.
                end if
            end if
        ! Diagonal capture
        else if (abs(x2 - x1) + abs(y2 - y1) == 1 .and. z2 == z1 + direction) then
            if (game%board(x2, y2, z2)%type /= EMPTY .and. game%board(x2, y2, z2)%color /= color) then
                valid = .true.
            end if
        end if
        
    end function is_valid_pawn_move
    
    function is_valid_knight_move(x1, y1, z1, x2, y2, z2) result(valid)
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        integer :: dx, dy, dz
        
        dx = abs(x2 - x1)
        dy = abs(y2 - y1)
        dz = abs(z2 - z1)
        
        valid = (dx + dy + dz == 3) .and. &
                (max(dx, max(dy, dz)) == 2) .and. &
                (min(dx, min(dy, dz)) == 1)
        
    end function is_valid_knight_move
    
    function is_valid_bishop_move(game, x1, y1, z1, x2, y2, z2) result(valid)
        type(GameState), intent(in) :: game
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        integer :: dx, dy, dz, i
        
        valid = .false.
        dx = x2 - x1
        dy = y2 - y1
        dz = z2 - z1
        
        ! Must move diagonally in 3D
        if (abs(dx) == abs(dy) .and. abs(dy) == abs(dz) .and. dx /= 0) then
            valid = .true.
            ! Check path is clear
            do i = 1, abs(dx) - 1
                if (game%board(x1 + i*dx/abs(dx), y1 + i*dy/abs(dy), z1 + i*dz/abs(dz))%type /= EMPTY) then
                    valid = .false.
                    exit
                end if
            end do
        end if
        
    end function is_valid_bishop_move
    
    function is_valid_rook_move(game, x1, y1, z1, x2, y2, z2) result(valid)
        type(GameState), intent(in) :: game
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        integer :: dx, dy, dz, i
        
        valid = .false.
        dx = x2 - x1
        dy = y2 - y1
        dz = z2 - z1
        
        ! Must move in straight line
        if ((dx == 0 .and. dy == 0 .and. dz /= 0) .or. &
            (dx == 0 .and. dy /= 0 .and. dz == 0) .or. &
            (dx /= 0 .and. dy == 0 .and. dz == 0)) then
            valid = .true.
            ! Check path is clear
            if (dx /= 0) then
                do i = 1, abs(dx) - 1
                    if (game%board(x1 + i*dx/abs(dx), y1, z1)%type /= EMPTY) then
                        valid = .false.
                        exit
                    end if
                end do
            else if (dy /= 0) then
                do i = 1, abs(dy) - 1
                    if (game%board(x1, y1 + i*dy/abs(dy), z1)%type /= EMPTY) then
                        valid = .false.
                        exit
                    end if
                end do
            else
                do i = 1, abs(dz) - 1
                    if (game%board(x1, y1, z1 + i*dz/abs(dz))%type /= EMPTY) then
                        valid = .false.
                        exit
                    end if
                end do
            end if
        end if
        
    end function is_valid_rook_move
    
    function is_valid_queen_move(game, x1, y1, z1, x2, y2, z2) result(valid)
        type(GameState), intent(in) :: game
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        
        ! Queen can move like rook or bishop
        valid = is_valid_rook_move(game, x1, y1, z1, x2, y2, z2) .or. &
                is_valid_bishop_move(game, x1, y1, z1, x2, y2, z2)
        
    end function is_valid_queen_move
    
    function is_valid_king_move(x1, y1, z1, x2, y2, z2) result(valid)
        integer, intent(in) :: x1, y1, z1, x2, y2, z2
        logical :: valid
        
        valid = (abs(x2 - x1) <= 1 .and. abs(y2 - y1) <= 1 .and. abs(z2 - z1) <= 1) .and. &
                (abs(x2 - x1) + abs(y2 - y1) + abs(z2 - z1) > 0)
        
    end function is_valid_king_move
    
    subroutine make_move(game, move_data)
        type(GameState), intent(inout) :: game
        type(Move), intent(in) :: move_data
        type(Move), allocatable :: temp_history(:)
        integer :: i
        
        ! Move piece
        game%board(move_data%x2, move_data%y2, move_data%z2) = game%board(move_data%x1, move_data%y1, move_data%z1)
        game%board(move_data%x1, move_data%y1, move_data%z1) = Piece(EMPTY, 0)
        
        ! Mark piece as moved
        game%board(move_data%x2, move_data%y2, move_data%z2)%has_moved = .true.
        
        ! Update game state
        game%move_count = game%move_count + 1
        game%current_player = merge(BLACK, WHITE, game%current_player == WHITE)
        
        ! Add to move history
        if (allocated(game%move_history)) then
            allocate(temp_history(size(game%move_history) + 1))
            do i = 1, size(game%move_history)
                temp_history(i) = game%move_history(i)
            end do
            temp_history(size(game%move_history) + 1) = move_data
            deallocate(game%move_history)
            game%move_history = temp_history
        else
            allocate(game%move_history(1))
            game%move_history(1) = move_data
        end if
        
    end subroutine make_move
    
    subroutine initialize_board(game)
        type(GameState), intent(out) :: game
        integer :: i, j, k
        
        ! Clear board
        game%board = Piece(EMPTY, 0)
        
        ! Place pieces for both players on different levels
        ! White pieces on level 1-2
        ! Black pieces on level 7-8
        
        ! Pawns
        do i = 1, BOARD_SIZE
            do j = 1, BOARD_SIZE
                game%board(i, j, 2) = Piece(PAWN, WHITE)
                game%board(i, j, 7) = Piece(PAWN, BLACK)
            end do
        end do
        
        ! Other pieces
        ! Corners - Rooks
        game%board(1, 1, 1) = Piece(ROOK, WHITE)
        game%board(BOARD_SIZE, 1, 1) = Piece(ROOK, WHITE)
        game%board(1, BOARD_SIZE, 1) = Piece(ROOK, WHITE)
        game%board(BOARD_SIZE, BOARD_SIZE, 1) = Piece(ROOK, WHITE)
        
        game%board(1, 1, 8) = Piece(ROOK, BLACK)
        game%board(BOARD_SIZE, 1, 8) = Piece(ROOK, BLACK)
        game%board(1, BOARD_SIZE, 8) = Piece(ROOK, BLACK)
        game%board(BOARD_SIZE, BOARD_SIZE, 8) = Piece(ROOK, BLACK)
        
        ! Next to corners - Knights
        game%board(2, 1, 1) = Piece(KNIGHT, WHITE)
        game%board(1, 2, 1) = Piece(KNIGHT, WHITE)
        game%board(BOARD_SIZE-1, 1, 1) = Piece(KNIGHT, WHITE)
        game%board(BOARD_SIZE, 2, 1) = Piece(KNIGHT, WHITE)
        game%board(2, BOARD_SIZE, 1) = Piece(KNIGHT, WHITE)
        game%board(1, BOARD_SIZE-1, 1) = Piece(KNIGHT, WHITE)
        game%board(BOARD_SIZE-1, BOARD_SIZE, 1) = Piece(KNIGHT, WHITE)
        game%board(BOARD_SIZE, BOARD_SIZE-1, 1) = Piece(KNIGHT, WHITE)
        
        game%board(2, 1, 8) = Piece(KNIGHT, BLACK)
        game%board(1, 2, 8) = Piece(KNIGHT, BLACK)
        game%board(BOARD_SIZE-1, 1, 8) = Piece(KNIGHT, BLACK)
        game%board(BOARD_SIZE, 2, 8) = Piece(KNIGHT, BLACK)
        game%board(2, BOARD_SIZE, 8) = Piece(KNIGHT, BLACK)
        game%board(1, BOARD_SIZE-1, 8) = Piece(KNIGHT, BLACK)
        game%board(BOARD_SIZE-1, BOARD_SIZE, 8) = Piece(KNIGHT, BLACK)
        game%board(BOARD_SIZE, BOARD_SIZE-1, 8) = Piece(KNIGHT, BLACK)
        
        ! Bishops
        game%board(3, 1, 1) = Piece(BISHOP, WHITE)
        game%board(1, 3, 1) = Piece(BISHOP, WHITE)
        game%board(BOARD_SIZE-2, 1, 1) = Piece(BISHOP, WHITE)
        game%board(BOARD_SIZE, 3, 1) = Piece(BISHOP, WHITE)
        game%board(3, BOARD_SIZE, 1) = Piece(BISHOP, WHITE)
        game%board(1, BOARD_SIZE-2, 1) = Piece(BISHOP, WHITE)
        game%board(BOARD_SIZE-2, BOARD_SIZE, 1) = Piece(BISHOP, WHITE)
        game%board(BOARD_SIZE, BOARD_SIZE-2, 1) = Piece(BISHOP, WHITE)
        
        game%board(3, 1, 8) = Piece(BISHOP, BLACK)
        game%board(1, 3, 8) = Piece(BISHOP, BLACK)
        game%board(BOARD_SIZE-2, 1, 8) = Piece(BISHOP, BLACK)
        game%board(BOARD_SIZE, 3, 8) = Piece(BISHOP, BLACK)
        game%board(3, BOARD_SIZE, 8) = Piece(BISHOP, BLACK)
        game%board(1, BOARD_SIZE-2, 8) = Piece(BISHOP, BLACK)
        game%board(BOARD_SIZE-2, BOARD_SIZE, 8) = Piece(BISHOP, BLACK)
        game%board(BOARD_SIZE, BOARD_SIZE-2, 8) = Piece(BISHOP, BLACK)
        
        ! Queens
        game%board(4, 4, 1) = Piece(QUEEN, WHITE)
        game%board(4, 4, 8) = Piece(QUEEN, BLACK)
        
        ! Kings
        game%board(5, 5, 1) = Piece(KING, WHITE)
        game%board(5, 5, 8) = Piece(KING, BLACK)
        
    end subroutine initialize_board
    
    subroutine display_board(game)
        type(GameState), intent(in) :: game
        integer :: i, j, k
        character(len=3) :: piece_char
        
        print *, "3D Chess Board - Level by Level"
        print *, "Current Player: ", merge("White", "Black", game%current_player == WHITE)
        print *, "Move Count: ", game%move_count
        print *, ""
        
        do k = 1, BOARD_SIZE
            print *, "Level ", k
            print *, "  1 2 3 4 5 6 7 8"
            do j = 1, BOARD_SIZE
                write(*, '(I2)', advance='no') j
                do i = 1, BOARD_SIZE
                    select case (game%board(i, j, k)%type)
                    case (EMPTY)
                        piece_char = " ."
                    case (PAWN)
                        piece_char = merge(" P", " p", game%board(i, j, k)%color == WHITE)
                    case (KNIGHT)
                        piece_char = merge(" N", " n", game%board(i, j, k)%color == WHITE)
                    case (BISHOP)
                        piece_char = merge(" B", " b", game%board(i, j, k)%color == WHITE)
                    case (ROOK)
                        piece_char = merge(" R", " r", game%board(i, j, k)%color == WHITE)
                    case (QUEEN)
                        piece_char = merge(" Q", " q", game%board(i, j, k)%color == WHITE)
                    case (KING)
                        piece_char = merge(" K", " k", game%board(i, j, k)%color == WHITE)
                    end select
                    write(*, '(A2)', advance='no') piece_char
                end do
                print *, ""
            end do
            print *, ""
        end do
        
    end subroutine display_board
    
end module chess_logic

module chess_ai
    use chess_types
    use chess_logic
    implicit none
    
contains
    function evaluate_position(game) result(score)
        type(GameState), intent(in) :: game
        real :: score
        integer :: i, j, k
        type(Piece) :: piece
        
        ! Piece values
        real, parameter :: piece_values(0:6) = [0.0, 1.0, 3.0, 3.0, 5.0, 9.0, 100.0]
        
        score = 0.0
        
        do k = 1, BOARD_SIZE
            do j = 1, BOARD_SIZE
                do i = 1, BOARD_SIZE
                    piece = game%board(i, j, k)
                    if (piece%type /= EMPTY) then
                        if (piece%color == WHITE) then
                            score = score + piece_values(piece%type)
                        else
                            score = score - piece_values(piece%type)
                        end if
                    end if
                end do
            end do
        end do
        
    end function evaluate_position
    
    function get_all_moves(game, player) result(moves)
        type(GameState), intent(in) :: game
        integer, intent(in) :: player
        type(Move), allocatable :: moves(:)
        type(Move), allocatable :: temp_moves(:)
        integer :: i, j, k, x, y, z, count
        
        allocate(temp_moves(1000))  ! Temporary large array
        count = 0
        
        do k = 1, BOARD_SIZE
            do j = 1, BOARD_SIZE
                do i = 1, BOARD_SIZE
                    if (game%board(i, j, k)%color == player) then
                        ! Get all possible moves for this piece
                        do z = 1, BOARD_SIZE
                            do y = 1, BOARD_SIZE
                                do x = 1, BOARD_SIZE
                                    if (is_valid_move(game, i, j, k, x, y, z)) then
                                        count = count + 1
                                        temp_moves(count) = Move(i, j, k, x, y, z, game%board(x, y, z))
                                    end if
                                end do
                            end do
                        end do
                    end if
                end do
            end do
        end do
        
        ! Copy to properly sized array
        allocate(moves(count))
        moves = temp_moves(1:count)
        deallocate(temp_moves)
        
    end function get_all_moves
    
    recursive function minimax(game, depth, alpha, beta, maximizing) result(score)
        type(GameState), intent(in) :: game
        integer, intent(in) :: depth
        real, intent(in) :: alpha, beta
        logical, intent(in) :: maximizing
        real :: score, best_score
        type(Move), allocatable :: moves(:)
        type(GameState) :: new_game
        integer :: i
        real :: new_alpha, new_beta
        
        if (depth == 0) then
            score = evaluate_position(game)
            return
        end if
        
        moves = get_all_moves(game, merge(WHITE, BLACK, maximizing))
        new_alpha = alpha
        new_beta = beta
        
        if (maximizing) then
            best_score = -10000.0
            do i = 1, size(moves)
                new_game = game
                call make_move(new_game, moves(i))
                score = minimax(new_game, depth - 1, new_alpha, new_beta, .false.)
                best_score = max(best_score, score)
                new_alpha = max(new_alpha, score)
                if (new_beta <= new_alpha) exit
            end do
        else
            best_score = 10000.0
            do i = 1, size(moves)
                new_game = game
                call make_move(new_game, moves(i))
                score = minimax(new_game, depth - 1, new_alpha, new_beta, .true.)
                best_score = min(best_score, score)
                new_beta = min(new_beta, score)
                if (new_beta <= new_alpha) exit
            end do
        end if
        
        score = best_score
        deallocate(moves)
        
    end function minimax
    
    function get_best_move(game, depth) result(best_move)
        type(GameState), intent(in) :: game
        integer, intent(in) :: depth
        type(Move) :: best_move
        type(Move), allocatable :: moves(:)
        type(GameState) :: new_game
        real :: best_score, score
        integer :: i, best_index
        
        moves = get_all_moves(game, game%current_player)
        best_score = -10000.0
        best_index = 1
        
        do i = 1, size(moves)
            new_game = game
            call make_move(new_game, moves(i))
            score = minimax(new_game, depth - 1, -10000.0, 10000.0, .false.)
            if (score > best_score) then
                best_score = score
                best_index = i
            end if
        end do
        
        best_move = moves(best_index)
        deallocate(moves)
        
    end function get_best_move
    
end module chess_ai

program chess_3d
    use chess_types
    use chess_logic
    use chess_ai
    implicit none
    
    type(GameState) :: game
    type(Move) :: player_move
    integer :: choice, ai_depth
    logical :: game_over
    integer :: i, j, k
    
    print *, "=== 3D Chess in Fortran ==="
    print *, "1. Human vs Human"
    print *, "2. Human vs AI"
    print *, "3. AI vs AI"
    read *, choice
    
    if (choice == 2 .or. choice == 3) then
        print *, "Enter AI search depth (1-4 recommended):"
        read *, ai_depth
    end if
    
    call initialize_board(game)
    game_over = .false.
    
    do while (.not. game_over)
        call display_board(game)
        
        if (choice == 1 .or. (choice == 2 .and. game%current_player == WHITE)) then
            ! Human move
            print *, "Enter move (x1 y1 z1 x2 y2 z2):"
            read *, player_move%x1, player_move%y1, player_move%z1, player_move%x2, player_move%y2, player_move%z2
            
            if (is_valid_move(game, player_move%x1, player_move%y1, player_move%z1, &
                              player_move%x2, player_move%y2, player_move%z2)) then
                player_move%captured = game%board(player_move%x2, player_move%y2, player_move%z2)
                call make_move(game, player_move)
            else
                print *, "Invalid move! Try again."
                cycle
            end if
        else
            ! AI move
            print *, "AI is thinking..."
            player_move = get_best_move(game, ai_depth)
            print *, "AI moves: (", player_move%x1, ",", player_move%y1, ",", player_move%z1, ") -> (", &
                     player_move%x2, ",", player_move%y2, ",", player_move%z2, ")"
            call make_move(game, player_move)
        end if
        
        ! Simple game over check (no kings)
        game_over = .true.
        do k = 1, BOARD_SIZE
            do j = 1, BOARD_SIZE
                do i = 1, BOARD_SIZE
                    if (game%board(i, j, k)%type == KING) then
                        game_over = .false.
                        exit
                    end if
                end do
            end do
        end do
        
        if (game_over) then
            print *, "Game Over!"
            exit
        end if
        
    end do
    
    print *, "Thanks for playing 3D Chess!"
    
end program chess_3d
