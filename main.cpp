#define VIRGO_IMPLEMENTATION
#define INITIAL "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#include "virgo.h"
#include <iostream>

int main() {
    virgo::virgo_init();

    std::vector<uint32_t> legal_moves = {};

    virgo::Chessboard board = virgo::position_from_fen(INITIAL);
    std::cout << board << std::endl;

    virgo::get_legal_moves<WHITE>(board, legal_moves);

    for(uint32_t & move : legal_moves) {
        std::cout << virgo::string::move_to_string(move) << std::endl;
    }

    std::cout << "total moves: " << legal_moves.size() << std::endl;

    return 0;
}
