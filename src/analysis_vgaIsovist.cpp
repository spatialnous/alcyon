// SPDX-FileCopyrightText: 2024 Petros Koutsolampros
//
// SPDX-License-Identifier: GPL-3.0-only

#include "salalib/vgamodules/vgaisovist.h"

#include "helper_nullablevalue.h"
#include "helper_runAnalysis.h"

#include "communicator.h"

#include <Rcpp.h>

// [[Rcpp::export("Rcpp_VGA_isovist")]]
Rcpp::List vgaIsovist(Rcpp::XPtr<PointMap> mapPtr, Rcpp::XPtr<ShapeMap> shapeMapPtr,
                      const Rcpp::Nullable<bool> copyMapNV = R_NilValue,
                      const Rcpp::Nullable<bool> progressNV = R_NilValue) {
    auto copyMap = NullableValue::get(copyMapNV, true);
    auto progress = NullableValue::get(progressNV, false);

    mapPtr = RcppRunner::copyMapWithRegion(mapPtr, copyMap);

    return RcppRunner::runAnalysis<PointMap>(
        mapPtr, progress, [&shapeMapPtr](Communicator *comm, Rcpp::XPtr<PointMap> mapPtr) {
            auto shapeMap = shapeMapPtr->getAllShapes();

            std::vector<SalaShape> shapes;
            shapes.reserve(shapeMap.size());
            for (auto it = shapeMap.begin(); it != shapeMap.end(); ++it) {
                shapes.push_back(it->second);
            }

            auto analysis = VGAIsovist(*mapPtr, shapes);
            auto analysisResult = analysis.run(comm);
            analysis.copyResultToMap(analysisResult.getAttributes(),
                                     std::move(analysisResult.getAttributeData()), *mapPtr,
                                     analysisResult.columnStats);
            return analysisResult;
        });
}
