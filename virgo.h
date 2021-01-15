/*
--- MIT License -------------------------------------------------------------
Copyright (c) 2021 Gianmarco Picarella

Permission is hereby granted, free of charge, to any person obtaining a copy
        of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
        copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
        copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-----------------------------------------------------------------------------
*/

/////////////////////////////////////////////////////////////////////// VIRGO HEADER ///////////////////////////////////////////////////////////////////////
#ifndef VIRGO_H
#define VIRGO_H

#include <cstdint>
#include <vector>
#include <regex>
#include <iostream>

#define ENCODE_MOVE(from, to, type) (0x0000u | (from) | ((to) << 6) | ((type) << 12))
#define MOVE_FROM(move) ((move) & 0x3f)
#define MOVE_TO(move) (((move) >> 6) & 0x3f)
#define MOVE_TYPE(move) (((move) >> 12) & 0xf)

namespace {
    uint64_t KINDERGARTEN[8][256];
    uint64_t KINDERGARTEN_ROTATED[8][256];
    uint64_t SQUARE_MASK[65];
    uint64_t FROM_TO_MASK[64][64];
    uint64_t LINE_MASK[64][64];

    const uint64_t DEBRUIJN_MAGIC = 0x03f79d71b4cb0a89ull;
    const uint8_t DEBRUIJN_INDICES[64] = {
            0, 47,  1, 56, 48, 27,  2, 60,
            57, 49, 41, 37, 28, 16,  3, 61,
            54, 58, 35, 52, 50, 42, 21, 44,
            38, 32, 29, 23, 17, 11,  4, 62,
            46, 55, 26, 59, 40, 36, 15, 53,
            34, 51, 20, 43, 31, 22, 10, 45,
            25, 39, 14, 33, 19, 30,  9, 24,
            13, 18,  8, 12,  7,  6,  5, 63
    };

    const uint64_t MAIN_DIAGONAL_MASK[64] = {
            0x8040201008040201, 0x80402010080402, 0x804020100804, 0x8040201008, 0x80402010, 0x804020, 0x8040, 0x0,
            0x4020100804020100, 0x8040201008040201, 0x80402010080402, 0x804020100804, 0x8040201008, 0x80402010, 0x804020, 0x8040,
            0x2010080402010000, 0x4020100804020100, 0x8040201008040201, 0x80402010080402, 0x804020100804, 0x8040201008, 0x80402010, 0x804020, 0x1008040201000000,
            0x2010080402010000, 0x4020100804020100, 0x8040201008040201, 0x80402010080402, 0x804020100804, 0x8040201008, 0x80402010, 0x804020100000000,
            0x1008040201000000, 0x2010080402010000, 0x4020100804020100, 0x8040201008040201, 0x80402010080402, 0x804020100804, 0x8040201008,0x402010000000000,
            0x804020100000000, 0x1008040201000000, 0x2010080402010000, 0x4020100804020100, 0x8040201008040201,0x80402010080402, 0x804020100804, 0x201000000000000,
            0x402010000000000, 0x804020100000000, 0x1008040201000000, 0x2010080402010000, 0x4020100804020100, 0x8040201008040201, 0x80402010080402, 0x0,
            0x201000000000000, 0x402010000000000, 0x804020100000000, 0x1008040201000000, 0x2010080402010000, 0x4020100804020100, 0x8040201008040201
    };

    const uint64_t MINOR_DIAGONAL_MASK[64] = {
            0x0, 0x102, 0x10204, 0x1020408, 0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080,
            0x102, 0x10204, 0x1020408, 0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080, 0x204081020408000,
            0x10204, 0x1020408, 0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080, 0x204081020408000, 0x408102040800000,
            0x1020408, 0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080, 0x204081020408000, 0x408102040800000, 0x810204080000000,
            0x102040810, 0x10204081020, 0x1020408102040, 0x102040810204080, 0x204081020408000, 0x408102040800000, 0x810204080000000, 0x1020408000000000,
            0x10204081020, 0x1020408102040, 0x102040810204080, 0x204081020408000, 0x408102040800000, 0x810204080000000, 0x1020408000000000, 0x2040800000000000,
            0x1020408102040, 0x102040810204080, 0x204081020408000, 0x408102040800000, 0x810204080000000, 0x1020408000000000, 0x2040800000000000, 0x4080000000000000,
            0x102040810204080, 0x204081020408000, 0x408102040800000, 0x810204080000000, 0x1020408000000000, 0x2040800000000000, 0x4080000000000000, 0x0
    };

    const uint64_t VERTICAL_MASK[64] = {
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
            0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080
    };

    const uint64_t HORIZONTAL_MASK[64] = {
            0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            0xff00, 0xff00, 0xff00, 0xff00, 0xff00, 0xff00, 0xff00, 0xff00,
            0xff0000, 0xff0000, 0xff0000, 0xff0000, 0xff0000, 0xff0000, 0xff0000, 0xff0000,
            0xff000000, 0xff000000, 0xff000000, 0xff000000, 0xff000000, 0xff000000, 0xff000000, 0xff000000,
            0xff00000000, 0xff00000000, 0xff00000000, 0xff00000000, 0xff00000000, 0xff00000000, 0xff00000000, 0xff00000000,
            0xff0000000000, 0xff0000000000, 0xff0000000000, 0xff0000000000, 0xff0000000000, 0xff0000000000, 0xff0000000000, 0xff0000000000,
            0xff000000000000, 0xff000000000000, 0xff000000000000, 0xff000000000000, 0xff000000000000, 0xff000000000000, 0xff000000000000, 0xff000000000000,
            0xff00000000000000, 0xff00000000000000, 0xff00000000000000, 0xff00000000000000, 0xff00000000000000, 0xff00000000000000, 0xff00000000000000, 0xff00000000000000
    };

    // Look-up table for the king
    const uint64_t KING_ATTACKS[64] = {
            0x302, 0x705, 0xe0a, 0x1c14,
            0x3828, 0x7050, 0xe0a0, 0xc040,
            0x30203, 0x70507, 0xe0a0e, 0x1c141c,
            0x382838, 0x705070, 0xe0a0e0, 0xc040c0,
            0x3020300, 0x7050700, 0xe0a0e00, 0x1c141c00,
            0x38283800, 0x70507000, 0xe0a0e000, 0xc040c000,
            0x302030000, 0x705070000, 0xe0a0e0000, 0x1c141c0000,
            0x3828380000, 0x7050700000, 0xe0a0e00000, 0xc040c00000,
            0x30203000000, 0x70507000000, 0xe0a0e000000, 0x1c141c000000,
            0x382838000000, 0x705070000000, 0xe0a0e0000000, 0xc040c0000000,
            0x3020300000000, 0x7050700000000, 0xe0a0e00000000, 0x1c141c00000000,
            0x38283800000000, 0x70507000000000, 0xe0a0e000000000, 0xc040c000000000,
            0x302030000000000, 0x705070000000000, 0xe0a0e0000000000, 0x1c141c0000000000,
            0x3828380000000000, 0x7050700000000000, 0xe0a0e00000000000, 0xc040c00000000000,
            0x203000000000000, 0x507000000000000, 0xa0e000000000000, 0x141c000000000000,
            0x2838000000000000, 0x5070000000000000, 0xa0e0000000000000, 0x40c0000000000000
    };

    // Look-up table for knights
    const uint64_t KNIGHT_ATTACKS[64] = {
            0x20400, 0x50800, 0xa1100, 0x142200,
            0x284400, 0x508800, 0xa01000, 0x402000,
            0x2040004, 0x5080008, 0xa110011, 0x14220022,
            0x28440044, 0x50880088, 0xa0100010, 0x40200020,
            0x204000402, 0x508000805, 0xa1100110a, 0x1422002214,
            0x2844004428, 0x5088008850, 0xa0100010a0, 0x4020002040,
            0x20400040200, 0x50800080500, 0xa1100110a00, 0x142200221400,
            0x284400442800, 0x508800885000, 0xa0100010a000, 0x402000204000,
            0x2040004020000, 0x5080008050000, 0xa1100110a0000, 0x14220022140000,
            0x28440044280000, 0x50880088500000, 0xa0100010a00000, 0x40200020400000,
            0x204000402000000, 0x508000805000000, 0xa1100110a000000, 0x1422002214000000,
            0x2844004428000000, 0x5088008850000000, 0xa0100010a0000000, 0x4020002040000000,
            0x400040200000000, 0x800080500000000, 0x1100110a00000000, 0x2200221400000000,
            0x4400442800000000, 0x8800885000000000, 0x100010a000000000, 0x2000204000000000,
            0x4020000000000, 0x8050000000000, 0x110a0000000000, 0x22140000000000,
            0x44280000000000, 0x0088500000000000, 0x0010a00000000000, 0x20400000000000
    };

    // Look-up table for white pawns
    const uint64_t WHITE_PAWN_ATTACKS[64] = {
            0x200, 0x500, 0xa00, 0x1400,
            0x2800, 0x5000, 0xa000, 0x4000,
            0x20000, 0x50000, 0xa0000, 0x140000,
            0x280000, 0x500000, 0xa00000, 0x400000,
            0x2000000, 0x5000000, 0xa000000, 0x14000000,
            0x28000000, 0x50000000, 0xa0000000, 0x40000000,
            0x200000000, 0x500000000, 0xa00000000, 0x1400000000,
            0x2800000000, 0x5000000000, 0xa000000000, 0x4000000000,
            0x20000000000, 0x50000000000, 0xa0000000000, 0x140000000000,
            0x280000000000, 0x500000000000, 0xa00000000000, 0x400000000000,
            0x2000000000000, 0x5000000000000, 0xa000000000000, 0x14000000000000,
            0x28000000000000, 0x50000000000000, 0xa0000000000000, 0x40000000000000,
            0x200000000000000, 0x500000000000000, 0xa00000000000000, 0x1400000000000000,
            0x2800000000000000, 0x5000000000000000, 0xa000000000000000, 0x4000000000000000,
            0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0,
    };

    // Look-up table for black pawns
    const uint64_t BLACK_PAWN_ATTACKS[64] = {
            0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0,
            0x2, 0x5, 0xa, 0x14,
            0x28, 0x50, 0xa0, 0x40,
            0x200, 0x500, 0xa00, 0x1400,
            0x2800, 0x5000, 0xa000, 0x4000,
            0x20000, 0x50000, 0xa0000, 0x140000,
            0x280000, 0x500000, 0xa00000, 0x400000,
            0x2000000, 0x5000000, 0xa000000, 0x14000000,
            0x28000000, 0x50000000, 0xa0000000, 0x40000000,
            0x200000000, 0x500000000, 0xa00000000, 0x1400000000,
            0x2800000000, 0x5000000000, 0xa000000000, 0x4000000000,
            0x20000000000, 0x50000000000, 0xa0000000000, 0x140000000000,
            0x280000000000, 0x500000000000, 0xa00000000000, 0x400000000000,
            0x2000000000000, 0x5000000000000, 0xa000000000000, 0x14000000000000,
            0x28000000000000, 0x50000000000000, 0xa0000000000000, 0x40000000000000,
    };

    // Look-up for castling permissions
    const uint8_t CASTLE_PERM[64] = {
            11, 15, 15, 15, 3, 15, 15, 7,
            15, 15, 15, 15, 15, 15, 15, 15,
            15, 15, 15, 15, 15, 15, 15, 15,
            15, 15, 15, 15, 15, 15, 15, 15,
            15, 15, 15, 15, 15, 15, 15, 15,
            15, 15, 15, 15, 15, 15, 15, 15,
            15, 15, 15, 15, 15, 15, 15, 15,
            14, 15, 15, 15, 12, 15, 15, 13
    };

}

namespace virgo {

    typedef enum Player {
        BLACK = 0,
        WHITE = 1
    } Player;

    typedef enum Square {
        a1 = 0,b1,c1,d1,e1,f1,g1,h1,
        a2,b2,c2,d2,e2,f2,g2,h2,
        a3,b3,c3,d3,e3,f3,g3,h3,
        a4,b4,c4,d4,e4,f4,g4,h4,
        a5,b5,c5,d5,e5,f5,g5,h5,
        a6,b6,c6,d6,e6,f6,g6,h6,
        a7,b7,c7,d7,e7,f7,g7,h7,
        a8,b8,c8,d8,e8,f8,g8,h8,
        INVALID = 64
    } Square;

    typedef enum Piece {
        PAWN,
        ROOK,
        KNIGHT,
        BISHOP,
        KING,
        QUEEN,
        EMPTY
    } Piece;

    enum MoveType {
        PAWN_QUIET,
        PAWN_DOUBLE,
        QUIET,
        CAPTURE,
        EN_PASSANT,
        CASTLE,
        PQ_R, PQ_B, PQ_Q, PQ_N,
        PC_R, PC_B, PC_Q, PC_N
    };

    enum Direction {
        NORTH,
        SOUTH,
        N_EAST,
        N_WEST,
        S_WEST,
        S_EAST
    };

    typedef struct HistoryMove {
        Piece capture;
        uint16_t move;
        unsigned int fifty_mv_counter = 0;
        uint64_t enpassant = INVALID;
        uint8_t castling_perm = 0;
    } HistoryMove;

    // Class maintaining information about the current board configuration
    class Chessboard {
    public:
        Chessboard();
        Chessboard(Chessboard const &c);

        std::pair<Piece, Player> & operator[] (unsigned int square) {
            return this->squares[square];
        }

        const std::pair<Piece, Player> & operator[] (unsigned int square) const {
            return this->squares[square];
        }

        // Given a piece type and a player, it returns the corresponding bitboard
        template<Player P> uint64_t get_bitboard(Piece piece) {
            return this->pieces[P][piece];
        }

        // Given a player, it returns the corresponding king position
        template <Player P> unsigned int king_square() {
            return this->king_position[P];
        }

        // Gieven a player it returns his occupancy bitboard
        template <Player P> uint64_t occupancy() const {
            return this->pieces[P][0] | this->pieces[P][1] | this->pieces[P][2] |
                   this->pieces[P][3] | this->pieces[P][4] | this->pieces[P][5];
        }

        // It returns the occupancy bitboard
        inline uint64_t occupancy() {
            return this->all;
        }

        // It returns the next player who has to make the next move
        inline Player get_next_to_move() {
            return this->next;
        }

        // It returns true if player P can castle king side otherwise false
        template <Player P> inline bool can_castle_king_side() {
            return this->castling_perm & (P == WHITE ? 0x08 : 0x02);
        }

        // It returns true if player P can castle queen side otherwise false
        template <Player P> inline bool can_castle_queen_side() {
            return this->castling_perm & (P == WHITE ? 0x04 : 0x01);
        }

    private:

        // It wipes a piece from a given square
        void clear_piece(unsigned int square);

        // It adds a new piece on to the given square
        void add_piece(Player player, Piece piece, unsigned int square);

        // It moves a piece from the "from" square to the "to" square
        void move_piece(unsigned int from, unsigned int to);

        // Player who has to make the next move
        Player next;

        // History move array
        std::vector<HistoryMove> history;
        unsigned int ply;

        // Castling permissions bits B0000KQkq (0x0f = full permissions)
        uint8_t castling_perm;

        // Fifty move rule counter (useful to evaluate 50 moves rule)
        uint8_t fifty_mv_counter;

        // Current enpassant square (from 0 to 63, 64 if it isn't set)
        unsigned int enpassant;

        // Black and white king positions for fast lookup
        unsigned int king_position[2];

        // Black pieces | White pieces together
        uint64_t all;

        // One bitboard for each piece type and color
        uint64_t pieces[2][6];

        // Fast lookup for piece and color for each board square
        std::pair<Piece, Player> squares[64];

        // Friends functions
        template <Player player> friend void get_legal_moves(Chessboard & board, std::vector<uint16_t> & mvs);
        template <Player player> friend void make_move(uint16_t move, Chessboard & board);
        template <Player player> friend void take_move(Chessboard & board);
        friend Chessboard position_from_fen(std::string fen);
    };

    // Given a FEN chess game representation it returns an equivalent, initialized Chessboard
    Chessboard position_from_fen(std::string fen);

    // Given a Chessboard object it reverts the latest move
    template <Player player> void take_move(Chessboard & board);

    // Given an encoded uint32_t move and a Chessboard object it makes the move
    template <Player player> bool make_move(uint32_t move, Chessboard & board);

    // Given a Chessboard object and an empty vector of uint32 it returns every legal move for the current (next to move) player
    template <Player player> void get_legal_moves(Chessboard & board, std::vector<uint32_t> & moves);

    // It initializes Virgo's Kindergarten lookup tables
    void virgo_init();
}
#endif

#ifdef VIRGO_IMPLEMENTATION
#undef VIRGO_IMPLEMENTATION

namespace {
    using namespace virgo;
/////////////////////////////////////////////////////////////////////// BIT MANIPULATION HELPERS ///////////////////////////////////////////////////////////////////////
    namespace bit {
        // It returns the most significant 1-bit index exploiting the de bruijn trick
        static inline uint8_t pop_msb_index(uint64_t & bb) {
            if(!bb) throw std::runtime_error("Undefined index when n = 0");
            bb |= bb >> 1;
            bb |= bb >> 2;
            bb |= bb >> 4;
            bb |= bb >> 8;
            bb |= bb >> 16;
            bb |= bb >> 32;
            return DEBRUIJN_INDICES[(bb * DEBRUIJN_MAGIC) >> 58];
        }

        // It Returns the least significant 1-bit index exploiting the de bruijn trick
        static inline uint8_t pop_lsb_index(uint64_t & bb) {
            if(!bb) throw std::runtime_error("Undefined index when n = 0");
            return DEBRUIJN_INDICES[((bb ^ (bb-1)) * DEBRUIJN_MAGIC) >> 58];
        }

        // It flips an uint64_t along its main diagonal
        static inline uint64_t flip_main_diagonal64(uint64_t x) {
            static const uint64_t k1 = 0x5500550055005500ull;
            static const uint64_t k2 = 0x3333000033330000ull;
            static const uint64_t k4 = 0x0f0f0f0f00000000ull;
            uint64_t t;
            t  = k4 & (x ^ (x << 28));
            x ^=       t ^ (t >> 28) ;
            t  = k2 & (x ^ (x << 14));
            x ^=       t ^ (t >> 14) ;
            t  = k1 & (x ^ (x <<  7));
            x ^=       t ^ (t >>  7) ;
            return x;
        }

        // It flips an uint64_t along its vertical line
        static inline uint64_t flip_vertical64(uint64_t x) {
            return  ((x << 56)                     ) |
                    ((x << 40) & 0x00ff000000000000) |
                    ((x << 24) & 0x0000ff0000000000) |
                    ((x <<  8) & 0x000000ff00000000) |
                    ((x >>  8) & 0x00000000ff000000) |
                    ((x >> 24) & 0x0000000000ff0000) |
                    ((x >> 40) & 0x000000000000ff00) |
                    ((x >> 56));
        }

        // It rotates an uint64_t 90 degrees counter clockwise
        static inline uint64_t rotate_counter_clockwise64(uint64_t x) {
            return flip_main_diagonal64(flip_vertical64(x));
        }

        // It repeats the first byte 8 times and returns the result as an uint64_t
        static inline uint64_t repeat_first_byte(uint64_t x) {
            x |= x << 8;
            x |= x << 16;
            x |= x << 32;
            return x;
        }

        // It shifts the bitboard following the direction which was given
        template <Direction D> uint64_t shift(uint64_t bb) {
            switch (D) {
                case NORTH:
                    return (bb << 8);
                case SOUTH:
                    return (bb >> 8);
                case N_EAST:
                    return (bb << 7);
                case S_EAST:
                    return (bb >> 9);
                case N_WEST:
                    return (bb << 9);
                case S_WEST:
                    return (bb >> 7);
            }
        }

        // Given a bitboard it returns the number of bits equal to one
        inline uint8_t hamming_weight(uint64_t bb) {
            uint8_t count = 0;
            while(bb && ++count) bb &= (bb-1);
            return count;
        }

        // Given a square it returns its main diagonal's index
        inline uint8_t main_diagonal_index(unsigned int square) {
            return 7 - (square & 0x7) + (square >> 3);
        }

        // Given a square it returns its minor diagonal's index
        inline uint8_t minor_diagonal_index(unsigned int square) {
            return (square & 0x7) + (square >> 3);
        }

    }
/////////////////////////////////////////////////////////////////////// BINARY PERMUTATIONS HELPERS ////////////////////////////////////////////////////////////////////
    namespace permutations {
        // It finds every binary permutation of length len and saving them inside a vector
        static inline void find_binary_permutations(uint8_t permutation, int len, std::vector<uint8_t> & permutations) {
            if(len == 0) {
                permutations.push_back(permutation);
                return;
            }
            find_binary_permutations(permutation, len - 1, permutations);
            find_binary_permutations(permutation | (1 << (len-1)), len - 1, permutations);
        }
    }
/////////////////////////////////////////////////////////////////////// STRING HELPERS /////////////////////////////////////////////////////////////////////////////////
    namespace string {
        // It trims spaces from a string at its left and right sides
        inline void trim(std::string & string) {
            string.erase(string.begin(), std::find_if_not(string.begin(), string.end(), [](uint8_t c){
                return std::isspace(c);
            }));
            string.erase(std::find_if_not(string.rbegin(), string.rend(), [](uint8_t c) {
                return std::isspace(c);
            }).base(), string.end());
        }
        // It returns a binary string representing an uint64_t
        inline std::string uint64ToString(uint64_t t) {
            std::string uint = {};
            uint64_t mask = 1ull << 63u;
            int count = 0;
            int c2 = 0;
            for(int i = 0; i < 64; i++) {
                uint.push_back(((mask & t) > 0) ? '1' : '0');
                if(++count == 4) {
                    uint.append(" | ");
                    count = 0;
                }
                if(++c2 == 8) {
                    uint.append("\n");
                    c2 = 0;
                }
                mask = mask >> 1u;
            }
            return uint;
        }
    }
/////////////////////////////////////////////////////////////////////// PSEUDO-LEGAL MOVES HELPERS /////////////////////////////////////////////////////////////////////
    namespace moves {
        // Given a pawn bitoboard and a player it returns the corresponding attacks toward east
        template <Player P> uint64_t pawns_attacks_east(uint64_t x) {
            const static uint64_t mask = 0xfefefefefefefefe;
            return P == WHITE ? bit::shift<N_EAST>(x&mask) : bit::shift<S_EAST>(x&mask);
        }

        // Given a pawn bitoboard and a player it returns the corresponding attacks toward west
        template <Player P> uint64_t pawns_attacks_west(uint64_t x) {
            const static uint64_t mask = 0x7f7f7f7f7f7f7f7f;
            return P == WHITE ? bit::shift<N_WEST>(x&mask) : bit::shift<S_WEST>(x&mask);
        }

        // Given a pawn bitoboard and a player it returns the corresponding forward moved bitboard
        template <Player P> uint64_t pawns_forward(uint64_t x) {
            return P == WHITE ? bit::shift<NORTH>(x) : bit::shift<SOUTH>(x);
        }

        // Given a pawn bitoboard and a player it returns the east and west attacks merged
        template <Player P> uint64_t getPawnsAttacks(uint64_t & pawnsBB) {
            return pawns_attacks_east<P>(pawnsBB) | pawns_attacks_west<P>(pawnsBB);
        }

        // Given a square it returns the positions from which a pawn could attack if there
        template<Player P> uint64_t get_pawns_attacks_to(unsigned int square) {
            return P == BLACK ? WHITE_PAWN_ATTACKS[square] : BLACK_PAWN_ATTACKS[square];
        }

        // Given an occupancy bitboard and a square it returns the corresponding set of diagonal attacks
        inline uint64_t diagonal_attacks(uint64_t occ, unsigned int square) {
            static const uint64_t a_file = 0x0101010101010101;

            uint64_t attacks = 0ull, index;

            index = (MAIN_DIAGONAL_MASK[square] & occ) * a_file >> 56;
            attacks |= MAIN_DIAGONAL_MASK[square] & KINDERGARTEN[square&7][index];
            index = (MINOR_DIAGONAL_MASK[square] & occ) * a_file >> 56;
            attacks |= MINOR_DIAGONAL_MASK[square] & KINDERGARTEN[square&7][index];

            return attacks;
        }

        // Given an occupancy bitboard and a square it returns the corresponding set of orthogonal attacks
        inline uint64_t orthogonal_attacks(uint64_t occ, unsigned int square) {
            static const uint64_t a_file = 0x0101010101010101;
            static const uint64_t main_diagonal = 0x0102040810204080;

            uint64_t attacks = 0ull, index;

            index = (HORIZONTAL_MASK[square] & occ) * a_file >> 56;
            attacks |= HORIZONTAL_MASK[square] & KINDERGARTEN[square&7][index];
            index = a_file & (occ >> (square & 0x7));
            index = (main_diagonal * index) >> 56;
            attacks |= VERTICAL_MASK[square] & (KINDERGARTEN_ROTATED[square>>3][index] << (square&7));

            return attacks;
        }

        // Given a player and a board it returns the attacked bitboard
        template <Player P> uint64_t get_attack_bitboard(uint64_t all_bb, Chessboard & board){
            uint64_t danger, pieces = board.get_bitboard<P>(PAWN);

            danger = getPawnsAttacks<P>(pieces) | KING_ATTACKS[board.king_square<P>()];

            pieces = board.occupancy<P>() & (~pieces);

            while(pieces) {
                unsigned int square = bit::pop_lsb_index(pieces);
                switch (board[square].first) {
                    case KNIGHT:
                        danger |= KNIGHT_ATTACKS[square];
                        break;
                    case ROOK:
                        danger |= orthogonal_attacks(all_bb, square);
                        break;
                    case BISHOP:
                        danger |= diagonal_attacks(all_bb, square);
                        break;
                    case QUEEN:
                        danger |= (diagonal_attacks(all_bb, square) | orthogonal_attacks(all_bb, square));
                        break;
                }
                pieces &= (pieces-1);
            }
            return danger;
        }
    }
}

namespace virgo {

    // Default constructor which initializes to the initial chess configuration
    Chessboard::Chessboard() {
        // Initial chessboard setup
        this->castling_perm = 0x00;
        this->fifty_mv_counter = 0;
        this->next = WHITE;
        this->enpassant = INVALID;
        this->king_position[0] = e8;
        this->king_position[1] = e1;

        // Set chess starting position
        this->all = 0xffff00000000ffff;

        // Set black pieces
        this->pieces[0][PAWN] = 0x00ff000000000000; // Pawns
        this->pieces[0][ROOK] = 0x8100000000000000; // Rooks
        this->pieces[0][KNIGHT] = 0x4200000000000000; // Knights
        this->pieces[0][BISHOP] = 0x2400000000000000; // Bishops
        this->pieces[0][KING] = 0x0800000000000000; // King
        this->pieces[0][QUEEN] = 0x1000000000000000; // Queen

        // Set white pieces
        this->pieces[1][PAWN] = 0x000000000000ff00; // Pawns
        this->pieces[1][ROOK] = 0x0000000000000081; // Rooks
        this->pieces[1][KNIGHT] = 0x0000000000000042; // Knights
        this->pieces[1][BISHOP] = 0x0000000000000024; // Bishops
        this->pieces[1][KING] = 0x0000000000000008; // King
        this->pieces[1][QUEEN] = 0x0000000000000010; // Queen

        // Fill with empty values
        for(int s = a1; s <= h8; s++) {
            this->squares[s] = std::make_pair(EMPTY, BLACK);
        }

        // Set every pair which has a piece on the corresponding square index
        for(int i = PAWN; i <= QUEEN; i++) {
            uint64_t board = this->pieces[BLACK][i];
            while(board) {
                this->squares[bit::pop_lsb_index(board)] = std::make_pair(static_cast<Piece>(i), BLACK);
                board &= (board-1);
            }
            board = this->pieces[WHITE][i];
            while(board) {
                this->squares[bit::pop_lsb_index(board)] = std::make_pair(static_cast<Piece>(i), WHITE);
                board &= (board-1);
            }
        }
    }

    // Copy constructor
    Chessboard::Chessboard(Chessboard const &c) {
        this->all = c.all;
        this->castling_perm = c.castling_perm;
        this->fifty_mv_counter = c.fifty_mv_counter;
        this->next = c.next;
        this->enpassant = c.enpassant;

        memcpy(this->pieces, c.pieces, 12*sizeof(uint64_t));

        for(int s = a1; s <= h8; s++) {
            this->squares[s] = c.squares[s];
        }

        this->king_position[0] = c.king_position[0];
        this->king_position[1] = c.king_position[1];
    }

    // Chessboard console format
    inline std::ostream & operator << (std::ostream & output, const Chessboard & board){
        static uint8_t pieceIcons[12] = {'p', 'r', 'n', 'b', 'k', 'q', 'P', 'R', 'N', 'B', 'K', 'Q'};

        std::string grid = "      A   B   C   D   E   F   G   H  \n";
        grid.append("    +---+---+---+---+---+---+---+---+ \n");

        for(int row = 0; row < 8; row++) {
            grid.push_back('8' - row);
            grid.append("   | ");
            for (int file = 0; file < 8; file++) {
                std::pair<Piece, Player> p = board[file + (7-row) * 8];
                if(p.first == EMPTY) grid.push_back(' ');
                else grid.push_back(pieceIcons[p.second * 6 + p.first]);
                grid.append(" | ");
            }
            grid.append("  ");
            grid.push_back('8' - row);
            grid.push_back('\n');
            grid.append("    +---+---+---+---+---+---+---+---+    \n");
        }

        grid.append("      A   B   C   D   E   F   G   H  \n");
        return output << grid;
    }

    // Given a FEN string it returns the corresponding Chessboard object
    Chessboard position_from_fen(std::string fen){
        const static std::map<char, Piece> indexes = {
                std::pair<char, Piece>('p', PAWN), std::pair<char, Piece>('r', ROOK),
                std::pair<char, Piece>('n', KNIGHT), std::pair<char, Piece>('b', BISHOP),
                std::pair<char, Piece>('k', KING), std::pair<char, Piece>('q', QUEEN)
        };

        // remove trailing spaces
        string::trim(fen);

        // check general format
        std::regex expr ("^([pnbrqkPNBRQK1-8]{1,8}\\/?){8}\\s(w|b)\\s(K?Q?k?q?|\\-)\\s([a-h][1-9]|\\-)\\s(0|[1-9][0-9]?)\\s(0|[1-9][0-9]?)$");

        if(!std::regex_match(fen, expr)) {
            throw std::runtime_error("Invalid FEN format");
        }

        // Create position object and setup
        Chessboard board = {};
        memset(board.pieces, 0ull, sizeof(board.pieces));
        board.castling_perm = 0x00;
        for(int s = a1; s <= h8; s++) board.squares[s] = std::make_pair(EMPTY, BLACK);


        // take just the board representation
        std::string board_str = fen.substr(0, fen.find(' '));

        int rowSum = 0;
        int square = 56;
        int kings[2] = {0, 0};

        for(char & c : board_str) {

            // End of row
            if(c == '/') {
                if(rowSum != 8) {
                    throw std::runtime_error("Invalid FEN format");
                }
                rowSum = 0;
                square -= 16;
                continue;
            }
            if(std::tolower(c) == 'k') {
                int color = std::isupper(c);
                kings[color]++;
            }

            if(std::isalpha(c)) {
                int color = std::isupper(c);
                board.pieces[color][indexes.at(std::tolower(c))] |= 1ull << square;
                board.squares[square++] = std::make_pair(indexes.at(std::tolower(c)), static_cast<Player>(color));

                rowSum++;
            }
            else {
                rowSum += c - '0';
                square += c - '0';
            }
        }

        // Invalid number of kings
        if(kings[0] != 1 || kings[1] != 1) {
            throw std::runtime_error("Invalid FEN format");
        }

        board.all = board.occupancy<WHITE>() | board.occupancy<BLACK>();
        board.king_position[0] = static_cast<Square>(bit::pop_lsb_index(board.pieces[0][KING]));
        board.king_position[1] = static_cast<Square>(bit::pop_lsb_index(board.pieces[1][KING]));

        // Take the rest of the string
        std::string rest = fen.substr(fen.find(' ') + 1);

        std::smatch match;
        expr.assign("^(w|b)\\s(K?Q?k?q?|\\-)\\s([a-h][1-9]|\\-)\\s(0|[1-9][0-9]?)\\s(0|[1-9][0-9]?)$");

        // Match the data
        std::regex_match(rest, match, expr);

        // Set next player to move
        board.next = match.str(1) == "w" ? WHITE : BLACK;

        // Set castling permissions
        std::string castlingPermissions = match.str(2);
        if(castlingPermissions != "-") {
            for(char c : castlingPermissions) {
                switch (c) {
                    case 'Q':
                        board.castling_perm |= 0x04;
                        break;
                    case 'K':
                        board.castling_perm |= 0x08;
                        break;
                    case 'q':
                        board.castling_perm |= 0x01;
                        break;
                    case 'k':
                        board.castling_perm |= 0x02;
                        break;
                }
            }

        }

        // Set enpassant position
        std::string enpassant = match.str(3);
        if(enpassant != "-") {
            board.enpassant = (enpassant[0] - 'a') + (enpassant[1] - '1') * 8;
        }

        // Fifty move rule counter
        board.fifty_mv_counter = std::stoi(match.str(4));

        return board;
    }

    // Given a player and a chessboard it reverts the last move made
    template <Player player> void take_move(Chessboard & board){
        static const int8_t EP_OFFSET[2] = { 8, -8 };

        // Get the last move made and remove it from the history
        HistoryMove last = board.history.back();
        board.history.pop_back();

        // Get the from and to square
        unsigned int from = MOVE_FROM(last.move),
                to = MOVE_TO(last.move);

        // Revert the board status
        board.next = player;
        board.castling_perm = last.castling_perm;
        board.fifty_mv_counter = last.fifty_mv_counter;
        board.enpassant = last.enpassant;
        board.ply--;

        // Revert the move based on what type it is
        switch (MOVE_TYPE(last.move)) {
            case EN_PASSANT:
                board.add_piece(static_cast<Player>(player ^ 1), PAWN, to + EP_OFFSET[player]);
                board.move_piece(to, from);
                break;
            case CASTLE:
                switch (to) {
                    case c1:
                        board.move_piece(d1, a1);
                        break;
                    case c8:
                        board.move_piece(d8, a8);
                        break;
                    case g1:
                        board.move_piece(f1, h1);
                        break;
                    case g8:
                        board.move_piece(f8, h8);
                        break;
                }
                board.move_piece(to, from);
                break;
            case CAPTURE:
                board.move_piece(to, from);
                board.add_piece(static_cast<Player>(player ^ 1), last.capture, to);
                break;
            case PQ_B:
            case PQ_R:
            case PQ_N:
            case PQ_Q:
                board.clear_piece(to);
                board.add_piece(player, PAWN, from);
                break;
            case PC_B:
            case PC_R:
            case PC_N:
            case PC_Q:
                board.clear_piece(to);
                board.add_piece(static_cast<Player>(player ^ 1), last.capture, to);
                board.add_piece(player, PAWN, from);
                break;
            default:
                board.move_piece(to, from);
        }
    }

    // Given a player, a move and a chessboard it makes the move
    template <Player player> void make_move(uint16_t move, Chessboard & board) {
        static const int8_t EP_OFFSET[2] = { 8, -8 };

        // Get the from and to square
        unsigned int from = MOVE_FROM(move),
                to = MOVE_TO(move);

        // Add the move and a set of board's variables which must be tracked
        board.history.push_back({board[to].first, move, board.fifty_mv_counter, board.enpassant, board.castling_perm});

        // Set the en-passant square to null
        board.enpassant = INVALID;

        // Make the move based on what type it is
        switch (MOVE_TYPE(move)) {
            case PAWN_QUIET:
                board.fifty_mv_counter = 0;
                board.move_piece(from, to);
                break;
            case PAWN_DOUBLE:
                board.fifty_mv_counter = 0;
                board.enpassant = to + EP_OFFSET[player];
                board.move_piece(from, to); // move pawn
                break;
            case EN_PASSANT:
                board.fifty_mv_counter = 0;
                board.clear_piece(to + EP_OFFSET[player]);
                board.move_piece(from, to); // move pawn
                break;
            case QUIET:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter++;
                board.move_piece(from, to);
                break;
            case CAPTURE:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(to);
                board.move_piece(from, to);
                break;
            case CASTLE:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter++;
                switch (to) {
                    case c1:
                        board.move_piece(a1, d1);
                        break;
                    case c8:
                        board.move_piece(a8, d8);
                        break;
                    case g1:
                        board.move_piece(h1, f1);
                        break;
                    case g8:
                        board.move_piece(h8, f8);
                        break;
                }
                board.move_piece(from, to);
                break;
            case PQ_B:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(from);
                board.add_piece(player, BISHOP, to);
                break;
            case PQ_N:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(from);
                board.add_piece(player, KNIGHT, to);
                break;
            case PQ_R:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(from);
                board.add_piece(player, ROOK, to);
                break;
            case PQ_Q:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(from);
                board.add_piece(player, QUEEN, to);
                break;
            case PC_B:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(to);
                board.clear_piece(from);
                board.add_piece(player, BISHOP, to);
                break;
            case PC_R:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(to);
                board.clear_piece(from);
                board.add_piece(player, ROOK, to);
                break;
            case PC_Q:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(to);
                board.clear_piece(from);
                board.add_piece(player, QUEEN, to);
                break;
            case PC_N:
                board.castling_perm &= CASTLE_PERM[from] & CASTLE_PERM[to];
                board.fifty_mv_counter = 0;
                board.clear_piece(to);
                board.clear_piece(from);
                board.add_piece(player, KNIGHT, to);
                break;
        }

        // Set the next player to move and increase the ply
        board.next = static_cast<Player>(player ^ 1);
        board.ply++;
    }

    // Given a player, a chessboard and a list of moves it fills the list with every legal move possible
    template <Player player> void get_legal_moves(Chessboard & board, std::vector<uint16_t> & mvs) {
        const static int8_t OFFSET[2][4] = {{-8,-7,-9,-16}, {8,9,7,16}};
        static const uint64_t PAWN_SPECIAL_RANK_MASK[2] = {0x00ff000000000000, 0x000000000000ff00};
        static const uint64_t CASTLING_ATTACK_MASK[2] = { 0x0c00000000000000, 0x000000000000000c };
        static const uint64_t CASTLING_EMPTY_MASK[2][2] = {
                {0x0e00000000000000, 0x000000000000000e},
                {0x6000000000000000, 0x0000000000000060}
        };

        // Define enemy player and some temporary variables
        constexpr Player enemy = static_cast<const Player>(player ^ 1);
        uint64_t b1, b2, b3, b4;

        // Find player's king square position
        unsigned int player_king_square = board.king_square<player>();

        // Define the occupancy bitboards for each player and its union
        uint64_t all_bb = board.occupancy();
        uint64_t enemy_bb = board.occupancy<enemy>();
        uint64_t player_bb = board.occupancy<player>();

        // Find pawns and knights player's bitboards
        uint64_t player_pawns_bb = board.get_bitboard<player>(PAWN);
        uint64_t player_knights_bb = board.get_bitboard<player>(KNIGHT);

        // Find orthogonal and diagonal pieces bitboards
        uint64_t enemy_queen_bb = board.get_bitboard<enemy>(QUEEN);
        uint64_t enemy_orth_bb = board.get_bitboard<enemy>(ROOK) | enemy_queen_bb;
        uint64_t enemy_diag_bb = board.get_bitboard<enemy>(BISHOP) | enemy_queen_bb;

        uint64_t player_queen_bb = board.get_bitboard<player>(QUEEN);
        uint64_t player_orth_bb = board.get_bitboard<player>(ROOK) | player_queen_bb;
        uint64_t player_diag_bb = board.get_bitboard<player>(BISHOP) | player_queen_bb;

        // Add enemy pawns and knights which check the player's king
        uint64_t checkers = (KNIGHT_ATTACKS[player_king_square] & board.get_bitboard<enemy>(KNIGHT)) |
                            (moves::get_pawns_attacks_to<enemy>(player_king_square) & board.get_bitboard<enemy>(PAWN));

        // Find enemy sliding pieces aligned with the player's king
        b1 = ((moves::orthogonal_attacks(enemy_bb, player_king_square)) & enemy_orth_bb) |
             ((moves::diagonal_attacks(enemy_bb, player_king_square)) & enemy_diag_bb);

        // Initialize the pinned bitboard
        uint64_t pinned = 0ull;

        // Remove the player's king to avoid problems with FROM_TO_MASK at **
        player_bb ^= SQUARE_MASK[player_king_square];

        // Iterate through every enemy slider piece aligned with the player's king
        while(b1) {
            // take the enemy sliding piece position
            unsigned int sliding_piece_square = bit::pop_lsb_index(b1);

            // take every piece between the range
            b2 = FROM_TO_MASK[player_king_square][sliding_piece_square] & player_bb;

            // if there are no pieces it is a checker
            if(b2 == 0) {
                checkers |= SQUARE_MASK[sliding_piece_square];
            }
                // otherwise if there is just one player's piece it is an absolutely pinned piece
            else if ((b2 & (b2-1)) == 0) {
                pinned |= b2;
            }
            b1 &= (b1-1);
        }

        // Find not-pinned pieces
        uint64_t not_pinned = (~pinned);

        // Add back the player's king
        player_bb ^= SQUARE_MASK[player_king_square];

        // get attacked squares by enemy pieces (without & not_enemy_pieces (it would make the program crash with some tests))
        /*
         * P - -
         * - k -
         * - - B
         * in this case if we had done an and operation with not_enemy_pieces we would have got
         * 0 - -
         * - 1 -
         * - - 1
         * giving the possibility for the king to move safe to the pawn position when actually it isn't safe
         */
        uint64_t attacked_bb = moves::get_attack_bitboard<enemy>(all_bb ^ SQUARE_MASK[player_king_square], board);
        b1 = attacked_bb;

        // find squares which are not under attack and leave the king out of check
        b2 = KING_ATTACKS[player_king_square] & (~b1);

        // Iterate through every possible quiet move square adding at each iteration a new move
        b3 = b2 & (~all_bb);
        while(b3) {
            mvs.push_back(ENCODE_MOVE(player_king_square, bit::pop_lsb_index(b3), QUIET));
            b3 &= (b3-1);
        }

        // Iterate through every possible attack move square adding at each iteration a new move
        b3 = b2 & (enemy_bb ^ board.get_bitboard<enemy>(KING));
        while(b3) {
            mvs.push_back(ENCODE_MOVE(player_king_square, bit::pop_lsb_index(b3), CAPTURE));
            b3 &= (b3-1);
        }

        // find the number of enemy pieces checking player's king
        unsigned int checkers_count = bit::hamming_weight(checkers);

        // If there are 2 checkers then return
        if(checkers_count > 1) return;
        // If there is one checker then capture or, if sliding piece, block its ray
        else if(checkers_count == 1) {

            // Find the square where the checking piece lies on and its type
            unsigned int checking_piece_square = bit::pop_lsb_index(checkers);
            Piece checker_piece = board[checking_piece_square].first;

            // If the checker has just double moved
            if(checker_piece == PAWN &&
               checkers == moves::pawns_forward<enemy>(SQUARE_MASK[board.enpassant])) {
                // Find all the pawns which aren't pinned and can attack the checker enpassant square
                b1 = moves::get_pawns_attacks_to<player>(board.enpassant) & player_pawns_bb & not_pinned;
                while(b1) {
                    mvs.push_back(ENCODE_MOVE(bit::pop_lsb_index(b1), board.enpassant, EN_PASSANT));
                    b1 &= (b1-1);
                }
            }

            // Find every player's piece which can attack the enemy checker
            b1 = moves::orthogonal_attacks(all_bb, checking_piece_square) & player_orth_bb & not_pinned;
            b1 |= moves::diagonal_attacks(all_bb, checking_piece_square) & player_diag_bb & not_pinned;
            b1 |= KNIGHT_ATTACKS[checking_piece_square] & player_knights_bb & not_pinned;

            b2 = moves::get_pawns_attacks_to<player>(checking_piece_square) & player_pawns_bb & not_pinned;

            // Find pawns which can attack with a promotion
            b3 = b2 & PAWN_SPECIAL_RANK_MASK[enemy];

            int square;

            // Add pawn's promotions attacks
            while(b3) {
                square = bit::pop_lsb_index(b3);
                mvs.push_back(ENCODE_MOVE(square, checking_piece_square, PC_B));
                mvs.push_back(ENCODE_MOVE(square, checking_piece_square, PC_R));
                mvs.push_back(ENCODE_MOVE(square, checking_piece_square, PC_N));
                mvs.push_back(ENCODE_MOVE(square, checking_piece_square, PC_Q));
                b3 &= (b3-1);
            }

            // Add pawns which can't be promoted but can still attack the checker
            b1 |= b2 & (~PAWN_SPECIAL_RANK_MASK[enemy]);

            // Add mixed pieces attack moves
            while(b1) {
                mvs.push_back(ENCODE_MOVE(bit::pop_lsb_index(b1), checking_piece_square, CAPTURE));
                b1 &= (b1-1);
            }

            // If the checker is a knight or pawn then return
            if(checker_piece == KNIGHT || checker_piece == PAWN) return;

            // Get the line between the king and the checker
            b1 = FROM_TO_MASK[player_king_square][checking_piece_square] ^
                 SQUARE_MASK[player_king_square] ^
                 SQUARE_MASK[checking_piece_square];

            // Copy the line
            b2 = b1;

            // Iterate through every line's square and add every move which can place a piece on to that
            while(b2) {
                square = bit::pop_lsb_index(b2);
                b3 = KNIGHT_ATTACKS[square] & player_knights_bb;
                b3 |= moves::orthogonal_attacks(all_bb, square) & player_orth_bb;
                b3 |= moves::diagonal_attacks(all_bb, square) & player_diag_bb;
                b3 &= ~pinned;
                while(b3) {
                    mvs.push_back(ENCODE_MOVE(bit::pop_lsb_index(b3), square, QUIET));
                    b3 &= (b3-1);
                }
                b2 &= (b2-1);
            }

            // Find single move pawns which fill the line
            b2 = moves::pawns_forward<player>(player_pawns_bb & not_pinned) & b1;

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square + OFFSET[enemy][0], square, PAWN_QUIET));
                b2 &= (b2-1);
            }

            // Find double move pawns which fill the line
            b2 = player_pawns_bb & PAWN_SPECIAL_RANK_MASK[player] & not_pinned;

            if(player == WHITE) b2 = (((b2 << 8) & ~all_bb) << 8) & ~all_bb & b1;
            else b2 = (((b2 >> 8) & ~all_bb) >> 8) & ~all_bb & b1;

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square + OFFSET[enemy][3], square, PAWN_DOUBLE));
                b2 &= (b2-1);
            }
        }
        // If there is no check
        else {
            unsigned int square;

            // Check if the player can castle queen side
            if(board.can_castle_queen_side<player>() && !(CASTLING_EMPTY_MASK[0][player] & all_bb) && !(attacked_bb & CASTLING_ATTACK_MASK[player])) {
                mvs.push_back(player == WHITE ? ENCODE_MOVE(e1, c1, CASTLE) : ENCODE_MOVE(e8, c8, CASTLE));
            }

            // Check if the player can castle king side
            if(board.can_castle_king_side<player>() && !(CASTLING_EMPTY_MASK[1][player] & (all_bb|attacked_bb))) {
                mvs.push_back(player == WHITE ? ENCODE_MOVE(e1, g1, CASTLE) : ENCODE_MOVE(e8, g8, CASTLE));
            }

            // If the en-passant is set then check if the player can capture the piece
            if(board.enpassant != INVALID) {
                // Pinned pawns first
                b1 = moves::get_pawns_attacks_to<player>(board.enpassant) & player_pawns_bb;
                b2 = b1 & pinned & LINE_MASK[board.enpassant][player_king_square];
                if(b2) mvs.push_back(ENCODE_MOVE(bit::pop_lsb_index(b2), board.enpassant, EN_PASSANT));

                // Not pinned pawns then
                b2 = b1 & not_pinned;
                // Find the piece bitboard mask
                b3 = SQUARE_MASK[board.enpassant + OFFSET[enemy][0]];

                while(b2) {
                    square = bit::pop_lsb_index(b2);
                    if(!(HORIZONTAL_MASK[player_king_square] &
                       moves::orthogonal_attacks(all_bb ^ SQUARE_MASK[square] ^ b3, player_king_square) & enemy_orth_bb)) {
                        mvs.push_back(ENCODE_MOVE(bit::pop_lsb_index(b2), board.enpassant, EN_PASSANT));
                    }
                    b2 &= (b2-1);
                }
            }

            // Find pinned rooks and queens
            b1 = (player_orth_bb | player_diag_bb) & pinned;

            // Add every quiet or attack move which is aligned with the king from pinned pieces
            while(b1) {
                square = bit::pop_lsb_index(b1);
                if(board[square].first == BISHOP) b2 = moves::diagonal_attacks(all_bb, square);
                else if(board[square].first == ROOK) b2 = moves::orthogonal_attacks(all_bb, square);
                else b2 = moves::diagonal_attacks(all_bb, square) | moves::orthogonal_attacks(all_bb, square);
                b2 &= LINE_MASK[square][player_king_square];
                b3 = b2 & (~all_bb);
                while(b3) {
                    mvs.push_back(ENCODE_MOVE(square, bit::pop_lsb_index(b3), QUIET));
                    b3 &= (b3-1);
                }
                b3 = b2 & enemy_bb;
                while(b3) {
                    mvs.push_back(ENCODE_MOVE(square, bit::pop_lsb_index(b3), CAPTURE));
                    b3 &= (b3-1);
                }
                b1 &= (b1-1);
            }

            // Find not pinned sliding pieces
            b1 = (player_diag_bb | player_orth_bb | player_knights_bb) & not_pinned;
            while(b1) {
                square = bit::pop_lsb_index(b1);
                if(board[square].first == BISHOP) b2 = moves::diagonal_attacks(all_bb, square);
                else if(board[square].first == ROOK) b2 = moves::orthogonal_attacks(all_bb, square);
                else if(board[square].first == QUEEN) b2 = moves::diagonal_attacks(all_bb, square) | moves::orthogonal_attacks(all_bb, square);
                else b2 = KNIGHT_ATTACKS[square];
                b3 = b2 & (~all_bb);
                while(b3) {
                    mvs.push_back(ENCODE_MOVE(square, bit::pop_lsb_index(b3), QUIET));
                    b3 &= (b3-1);
                }
                b3 = b2 & enemy_bb;
                while(b3) {
                    mvs.push_back(ENCODE_MOVE(square, bit::pop_lsb_index(b3), CAPTURE));
                    b3 &= (b3-1);
                }
                b1 &= (b1-1);
            }

            // Find pinned pawns
            b1 = player_pawns_bb & pinned;

            b3 = (player == WHITE ? MAIN_DIAGONAL_MASK[player_king_square] : MINOR_DIAGONAL_MASK[player_king_square]);
            b4 = (player == WHITE ? MINOR_DIAGONAL_MASK[player_king_square] : MAIN_DIAGONAL_MASK[player_king_square]);

            // A pinned pawn can promote just if its capture square is diagonally aligned with the king
            b2 = enemy_bb & moves::pawns_attacks_west<player>(b1 & PAWN_SPECIAL_RANK_MASK[enemy]) & b3;

            // First direction
            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_B));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_Q));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_N));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_R));
            }

            b2 = enemy_bb & moves::pawns_attacks_east<player>(b1 & PAWN_SPECIAL_RANK_MASK[enemy]) & b4;

            // Then the other one
            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_B));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_Q));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_N));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_R));
            }

            // Find pinned captures
            b2 = enemy_bb & moves::pawns_attacks_west<player>(b1 & ~PAWN_SPECIAL_RANK_MASK[enemy]) & b3;

            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, CAPTURE));
            }

            b2 = enemy_bb & moves::pawns_attacks_east<player>(b1 & ~PAWN_SPECIAL_RANK_MASK[enemy]) & b4;

            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, CAPTURE));
            }

            // Double pinned moves
            b2 = b1 & PAWN_SPECIAL_RANK_MASK[player];

            if(player == WHITE) b2 = (((b2 << 8) & ~all_bb) << 8) & ~all_bb;
            else b2 = (((b2 >> 8) & ~all_bb) >> 8) & ~all_bb;
            b2 &= VERTICAL_MASK[player_king_square];

            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][3] , square, PAWN_DOUBLE));
            }

            // Single pinned moves
            b2 = moves::pawns_forward<player>(b1) & (~all_bb) & VERTICAL_MASK[player_king_square];

            if(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0] , square, PAWN_QUIET));
            }

            // Find not pinned pawns
            b1 = player_pawns_bb & not_pinned & PAWN_SPECIAL_RANK_MASK[enemy];

            // Quiet promotions
            b2 = moves::pawns_forward<player>(b1) & (~all_bb);

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0], square, PQ_B));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0], square, PQ_Q));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0], square, PQ_N));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0], square, PQ_R));
                b2 &= (b2-1);
            }

            // Attack promotions
            b2 = moves::pawns_attacks_west<player>(b1) & enemy_bb;
            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_B));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_Q));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_N));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, PC_R));
                b2 &= (b2-1);
            }

            b2 = moves::pawns_attacks_east<player>(b1) & enemy_bb;
            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_B));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_Q));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_N));
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, PC_R));
                b2 &= (b2-1);
            }

            // Find not pinned pawns which can attack and aren't within the special mask rank
            b1 = player_pawns_bb & ~PAWN_SPECIAL_RANK_MASK[enemy] & not_pinned;

            // East attacks
            b2 = moves::pawns_attacks_east<player>(b1) & enemy_bb;

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][2], square, CAPTURE));
                b2 &= (b2-1);
            }

            // West attacks
            b2 = moves::pawns_attacks_west<player>(b1) & enemy_bb;

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][1], square, CAPTURE));
                b2 &= (b2-1);
            }

            // Find not pinned pawns which can double move
            b2 = b1 & PAWN_SPECIAL_RANK_MASK[player];

            if(player == WHITE) b2 = (((b2 << 8) & ~all_bb) << 8) & ~all_bb;
            else b2 = (((b2 >> 8) & ~all_bb) >> 8) & ~all_bb;

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][3] , square, PAWN_DOUBLE));
                b2 &= (b2-1);
            }

            // Find not pinned pawns which can make a quiet move
            b2 = moves::pawns_forward<player>(b1) & (~all_bb);

            while(b2) {
                square = bit::pop_lsb_index(b2);
                mvs.push_back(ENCODE_MOVE(square - OFFSET[player][0], square, PAWN_QUIET));
                b2 &= (b2-1);
            }
        }
    }

    // It moves a piece from the "from" square to the "to" square
    void Chessboard::move_piece(unsigned int from, unsigned int to) {
        auto & f = this->squares[from];
        auto & t = this->squares[to];

        this->pieces[f.second][f.first] &= ~(1ull << from);
        this->all &= ~(1ull << from);

        this->pieces[f.second][f.first] |= (1ull << to);
        this->all |= (1ull << to);

        t.first = f.first;
        t.second = f.second;
        f.first = EMPTY;

        if(t.first == KING) {
            this->king_position[t.second] = to;
        }
    }

    // It wipes a piece from a given square
    void Chessboard::clear_piece(unsigned int square) {
        auto & t = this->squares[square];
        this->pieces[t.second][t.first] &= ~(1ull << square);
        this->all &= ~(1ull << square);
        t.first = EMPTY;
    }

    // It adds a new piece on to the given square
    void Chessboard::add_piece(Player player, Piece piece, unsigned int square) {
        auto & t = this->squares[square];
        t.first = piece;
        t.second = player;
        this->pieces[player][piece] |= 1ull << square;
        this->all |= (1ull << square);
    }

    // It initializes Virgo's lookup tables
    void virgo_init() {

        // Kindergarten bitboards init
        std::vector<uint8_t> first_rank_permutations;
        permutations::find_binary_permutations(0, 8, first_rank_permutations);
        for(int i = 0; i < 8; i++) {
            for(uint8_t u : first_rank_permutations) {
                uint8_t result = 0;
                uint8_t uc = u & ~(1ull << i);
                uint64_t left = uc >> i;
                int index = left > 0 ? (bit::pop_lsb_index(left) + i) : 7;
                while(index > i) result |= (1 << index--);
                uint64_t right = (uc << (7 - i)) & 0xff;
                index = right > 0 ? (bit::pop_msb_index(right) - (7-i)) : 0;
                while(index < i) result |= (1 << index++);
                KINDERGARTEN[i][u] = bit::repeat_first_byte(result);
                KINDERGARTEN_ROTATED[i][u] = bit::rotate_counter_clockwise64(KINDERGARTEN[i][u]);
            }
        }

        // Square bitboards init
        SQUARE_MASK[64] = 0ull;
        for(int square = 0; square < 64; square++)
            SQUARE_MASK[square] = 1ull << square;

        // Lines and ranges bitboards masks init
        for(int s = 0; s < 64; s++) {
            for(int t = 0; t < 64; t++) {
                LINE_MASK[s][t] = 0ull;
                FROM_TO_MASK[s][t] = 0ull;
                if(s == t) continue;
                uint64_t mask = SQUARE_MASK[s] | SQUARE_MASK[t] | ((SQUARE_MASK[t] - 1) ^ (SQUARE_MASK[s] - 1));
                if((s & 0x7) == (t & 0x7) && (s >> 3) != (t >> 3)) {
                    FROM_TO_MASK[s][t] = mask & VERTICAL_MASK[s];
                    LINE_MASK[s][t] = VERTICAL_MASK[s];
                }
                else if((s & 0x7) != (t & 0x7) && (s >> 3) == (t >> 3)) {
                    FROM_TO_MASK[s][t] = mask & HORIZONTAL_MASK[s];
                    LINE_MASK[s][t] = HORIZONTAL_MASK[s];
                }
                else if(bit::main_diagonal_index(s) == bit::main_diagonal_index(t)) {
                    FROM_TO_MASK[s][t] = mask & MAIN_DIAGONAL_MASK[s];
                    LINE_MASK[s][t] = MAIN_DIAGONAL_MASK[s];
                }
                else if(bit::minor_diagonal_index(s) == bit::minor_diagonal_index(t)) {
                    FROM_TO_MASK[s][t] = mask & MINOR_DIAGONAL_MASK[s];
                    LINE_MASK[s][t] = MINOR_DIAGONAL_MASK[s];
                }
            }
        }
    }

/////////////////////////////////////////////////////////////////////// TEST HELPERS ///////////////////////////////////////////////////////////////////////////////////
    namespace test {
        // Recursive function useful when testing
        template <Player player> long int perft(int d, Chessboard & board) {
            if(d == 0) return 1;

            std::vector<uint16_t> moves;
            virgo::get_legal_moves<player>(board, moves);

            uint64_t total_moves_count = 0;
            constexpr Player enemy = static_cast<const Player>(player ^ 1);

            // For each legal move do it and go deeper
            for(uint16_t & move : moves) {

                // Make the move
                virgo::make_move<player>(move, board);

                uint64_t rec_count = perft<enemy>(d - 1, board);
                total_moves_count += rec_count;

                // At the end revert the move
                virgo::take_move<player>(board);
            }
            return total_moves_count;
        }
    }
/////////////////////////////////////////////////////////////////////// PUBLIC STRING HELPERS //////////////////////////////////////////////////////////////////////////
    namespace string {
        // Given a uint32_t representing a move it returns the corresponding string
        inline std::string move_to_string(uint16_t move) {
            const static char pieces[7] = {'p', 'r', 'n', 'b', 'k', 'q', '\0'};

            unsigned int f = MOVE_FROM(move);
            unsigned int t = MOVE_TO(move);

            std::string move_str = {};
            move_str.push_back('a' + (f & 0x7));
            move_str.push_back('1' + (f >> 3));
            move_str.push_back('a' + (t & 0x7));
            move_str.push_back('1' + (t >> 3));

            if(MOVE_TYPE(move) == PC_R || MOVE_TYPE(move) == PQ_R)
                move_str.push_back('r');
            else if(MOVE_TYPE(move) == PC_N || MOVE_TYPE(move) == PQ_N)
                move_str.push_back('n');
            else if(MOVE_TYPE(move) == PC_Q || MOVE_TYPE(move) == PQ_Q)
                move_str.push_back('q');
            else if(MOVE_TYPE(move) == PC_B || MOVE_TYPE(move) == PQ_B)
                move_str.push_back('b');

            return move_str;
        }
    }
}
#endif //VIRGO_IMPLEMENTATION