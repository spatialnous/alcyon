// SPDX-FileCopyrightText: 2024-2025 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "communicator.hpp"

std::unique_ptr<Communicator> getCommunicator(const bool printProgress) {
    if (printProgress) {
        return std::unique_ptr<Communicator>(new ProgressCommunicator(printProgress));
    }
    return nullptr;
}
