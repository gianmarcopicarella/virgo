#define VIRGO_IMPLEMENTATION
#include "virgo.h"
#include <iostream>

int main() {
    virgo::virgo_init();

    std::vector<uint16_t> legal_moves = {};

    virgo::Chessboard board = virgo::position_from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N w - - 0 1");
    std::cout << board << std::endl;

    virgo::get_legal_moves<WHITE>(board, legal_moves);

    for(uint16_t & move : legal_moves) {
        std::cout << virgo::string::move_to_string(move) << std::endl;
    }

    std::cout << "\ntotal moves: " << legal_moves.size() << std::endl;

    return 0;
}
