// Copyright 2024 Petros Koutsolampros
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "salalib/mgraph.h"
#include "salalib/tidylines.h"
#include "genlib/p2dpoly.h"

#include <Rcpp.h>
#include <typeinfo>
#include <exception>

// debian 12 install:
// sudo apt install libudunits2-dev libproj-dev libgdal-dev

// [[Rcpp::plugins(cpp17)]]

RCPP_EXPOSED_CLASS(ShapeMap);
RCPP_EXPOSED_CLASS(ShapeGraph);

// Simple class meant to hold only the data of the MetaGraph
struct MetaGraphData {
    std::vector<Rcpp::XPtr<ShapeMap>> shapeMaps;
    std::vector<Rcpp::XPtr<ShapeGraph>> shapeGraphs;
};

RCPP_EXPOSED_CLASS(MetaGraphData);

//*// [[Rcpp::export("getMetaGraph")]]
MetaGraphData readMetaGraph(std::string fileName) {
    Rcpp::Rcerr << "Loading MetaGraph at: " << fileName << std::endl;
    auto m = std::unique_ptr<MetaGraph>(new MetaGraph(fileName));
    m->readFromFile(fileName);
    Rcpp::Rcerr << "- bb: " << m->getBoundingBox().area() << std::endl;

    MetaGraphData mgraphData;
    for (auto & drawingFile : m->m_drawingFiles) {
        Rcpp::Rcerr << " - drawingName: "
                    << drawingFile.getName() << std::endl;
        for (auto &shapeMap : drawingFile.m_spacePixels) {
            Rcpp::Rcerr << " - shapeMapName: "
                        << shapeMap.getName() << std::endl;
            ShapeMap m(std::move(shapeMap));
            mgraphData.shapeMaps.push_back(Rcpp::XPtr<ShapeMap>(&m, true));
        }
    }
    return mgraphData;
}

namespace ShapeMapFuncs {
Rcpp::XPtr<ShapeMap> make(std::string name) {
    return Rcpp::XPtr<ShapeMap>(new ShapeMap(name), true);
}

std::string getName(Rcpp::XPtr<ShapeMap> shapeMap) {
    return shapeMap->getName();
}
std::vector<std::string> getAttributeNames(Rcpp::XPtr<ShapeMap> shapeMap) {
    auto &attributes = shapeMap->getAttributeTable();
    int numCols = attributes.getNumColumns();
    std::vector<std::string> names;
    // + 1 for the key column
    names.reserve(1 + numCols);
    names.push_back(attributes.getColumnName(size_t(-1)));
    for(int i = 0; i < attributes.getNumColumns(); ++i) {
        names.push_back(attributes.getColumnName(i));
    }
    return names;
}

std::vector<double> getAttributeData(Rcpp::XPtr<ShapeMap> shapeMap, std::string attributeName) {
    auto &attributes = shapeMap->getAttributeTable();
    std::vector<double> data;
    data.reserve(attributes.getNumRows());
    if (attributeName == attributes.getColumnName(size_t(-1))) {
        for(auto rowIt = attributes.begin(); rowIt != attributes.end(); ++rowIt) {
            data.push_back(rowIt->getKey().value);
        }
    } else {
        size_t colIdx = attributes.getColumnIndex(attributeName);
        for(auto rowIt = attributes.begin(); rowIt != attributes.end(); ++rowIt) {
            data.push_back(rowIt->getRow().getValue(colIdx));
        }
    }
    return data;
}
}


Rcpp::XPtr<ShapeMap> toShapeMap(Rcpp::DataFrame &df) {

    Rcpp::XPtr<ShapeMap> shp(new ShapeMap("tmp_df_shp"));
    for (auto it = df.begin(); it != df.end(); it++) {
        // https://gallery.rcpp.org/articles/rcpp-wrap-and-recurse/
        // #define INTSXP      13    /* integer vectors */
        // #define REALSXP     14    /* real variables */
        // #define CPLXSXP     15    /* complex variables */
        // #define STRSXP      16    /* string vectors */
        // #define DOTSXP      17    /* dot-dot-dot object */
        // #define ANYSXP      18    /* make "any" args work. */
        // #define VECSXP      19    /* generic vectors */
        switch( TYPEOF(*it) ) {
        case REALSXP: {
            auto tmp = Rcpp::as<Rcpp::NumericVector>(*it);

            break;
        }
        case STRSXP: {
            auto tmp = Rcpp::as<Rcpp::StringVector>(*it);

            break;
        }
        case INTSXP: {
            if( Rf_isFactor(*it) ) break; // factors have internal type INTSXP
            auto tmp = Rcpp::as<Rcpp::IntegerVector>(*it);
            break;
        }
        case VECSXP: {
            // generic vector
            auto gv = Rcpp::as<Rcpp::GenericVector>(*it);
            if (gv.hasAttribute("class") &&
                TYPEOF(gv.attr("class")) == STRSXP) {
                // has a class attribute which is a string vector
                auto classData = Rcpp::as<Rcpp::StringVector>(gv.attr("class"));
                bool isLineString = std::find(
                    classData.begin(), classData.end(),
                    "sfc_LINESTRING") != classData.end();

                if (isLineString) {
                    for (auto lit = gv.begin(); lit != gv.end(); ++lit) {
                        auto coords = Rcpp::as<Rcpp::NumericVector>(*lit);
                        if (coords.size() == 4) {
                            // 2D line x1,y1,x2,y2
                            shp->makeLineShape(
                                    Line(Point2f(coords[0], coords[1]),
                                         Point2f(coords[2], coords[3])));
                        }
                    }
                }
            }
            break;
        }
        default: {
            Rcpp::stop("incompatible SEXP encountered; only accepts lists"
                           " with REALSXPs, STRSXPs, VECSXPs and INTSXPs");
        }
        }
    }
    return shp;
}


Rcpp::XPtr<ShapeGraph> toAxialShapeGraph(Rcpp::XPtr<ShapeMap> shapeMap) {

    QtRegion region;
    // map required for tidy lines, otherwise acts like vector
    std::map<int, std::pair<Line, int>> lines;

    // add all visible layers to the set of polygon lines...
    int count = 0;

    if (region.atZero()) {
        region = shapeMap->getRegion();
    } else {
        region = runion(region, shapeMap->getRegion());
    }
    std::vector<SimpleLine> newLines = shapeMap->getAllShapesAsLines();
    for (const auto &line : newLines) {
        lines.insert(
            std::make_pair(count, std::make_pair(
                    Line(line.start(), line.end()), count)));
        count++;
    }

    if (count == 0) {
        // TODO: write a better error message
        throw depthmapX::RuntimeException("Failed to convert lines");
    }

    // quick tidy removes very short and duplicate lines,
    // but does not merge overlapping lines
    TidyLines tidier;
    tidier.quicktidy(lines, region);
    if (lines.size() == 0) {
        throw depthmapX::RuntimeException(
                "No lines found after removing short and duplicates");
    }

    Rcpp::XPtr<ShapeGraph> usermap(
            new ShapeGraph(shapeMap->getName() + "_axial", ShapeMap::AXIALMAP));

    usermap->init(int(lines.size()), region); // used to be double density
    std::map<int, float> layerAttributes;
    usermap->initialiseAttributesAxial();
    int layerCol = -1;
    for (auto &line : lines) {
        usermap->makeLineShape(line.second.first, false, false, layerAttributes);
    }

    usermap->makeConnections();

    return usermap;
}



// should not expose metagraph, instead only shapemaps/shapegraphs
// and use the metagraph just to import the shapemaps to an R list
RCPP_MODULE(aedon_module) {
    Rcpp::class_<MetaGraphData>("MetaGraphData")
    .field_readonly("shapeMaps", &MetaGraphData::shapeMaps)
    .field_readonly("shapeGraphs", &MetaGraphData::shapeGraphs)
    ;

    // Rcpp::class_<ShapeMap>("sala_ShapeMap")
    // .constructor<std::string, int>()
    // .method("getName", &ShapeMap::getName)
    // ;
    Rcpp::function("readMetaGraph", &readMetaGraph);
    Rcpp::function("getName", &ShapeMapFuncs::getName);
    Rcpp::function("makeShapeMap", &ShapeMapFuncs::make);
    Rcpp::function("getAttributeNames", &ShapeMapFuncs::getAttributeNames);
    Rcpp::function("getAttributeData", &ShapeMapFuncs::getAttributeData);


    Rcpp::function("toShapeMap", &toShapeMap);
    Rcpp::function("toAxialShapeGraph", &toAxialShapeGraph);
}


