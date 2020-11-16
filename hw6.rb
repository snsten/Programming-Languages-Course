# University of Washington, Programming Languages, Homework 6, hw6runner.rb
# This is the only file you turn in, so do not modify the other files as
# part of your solution.
class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [ 
                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), #2x3 Block
                rotations([[0,0], [0, 1], [1,1]]), # Corner Block
                [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], #5x1 Block
                [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]]] #5x1 Block

  # class array 
  attr_accessor :block_size

  def initialize(point_array, board)
    super(point_array, board)
    @block_size = point_array[0].size
  end

  # your enhancements here
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  attr_accessor :score, :cheats

  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @cheats = false
    @score = 0
    @game = game
    @delay = 500
  end

  def check_cheat
    if @score >= 100 and !@cheat
      @score -= 100
      @cheat = true
    end
  end

  def next_piece
    if @cheats 
      @current_block = MyPiece.new([[0, 0]], self)
    else
      @current_block = MyPiece.next_piece(self)
    end

    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    block_size = @current_block.block_size
    (0..block_size - 1).each { |index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
    @root.bind("u", proc { @board.rotate_clockwise; @board.rotate_clockwise })
    @root.bind("c", proc { @board.check_cheat })
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3, @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end
