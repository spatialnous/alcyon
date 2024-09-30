// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#pragma once

#include "salalib/genlib/comm.h"

#include <Rcpp.h>

#include <cli/progress.h>

#include <memory>

class ProgressCommunicator : public Communicator {
    mutable SEXP progress = nullptr;

  public:
    ProgressCommunicator(bool displayProgress = false) {}
    ~ProgressCommunicator() {
        cli_progress_done(progress);
        UNPROTECT(1);
    }

    void CommPostMessage(size_t m, size_t x) const override {
        try {
            Rcpp::checkUserInterrupt();
        } catch (Rcpp::internal::InterruptedException &e) {
            m_cancelled = true;
            return;
        }

        if (m == Communicator::NUM_RECORDS && x > 0) {
            progress = PROTECT(cli_progress_bar(x, NULL));

        } else if (CLI_SHOULD_TICK && m == Communicator::CURRENT_RECORD) {
            cli_progress_set(progress, x);
        }
    }
};

std::unique_ptr<Communicator> getCommunicator(const bool printProgress);
