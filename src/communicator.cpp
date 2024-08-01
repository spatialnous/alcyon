// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "communicator.h"

std::unique_ptr<Communicator> getCommunicator(const bool printProgress) {
    if (printProgress) {
        return std::unique_ptr<Communicator>(new ProgressCommunicator(printProgress));
    }
    return nullptr;
}
