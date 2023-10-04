package persistence;

import Constants.ProjectConfig;
import logger.ConsoleWriter;
import logger.Logger;
import model.MapModel;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class OpenMap {

    private Logger d_logger;
    private BufferedReader d_Buffer;
    private String d_line;

    private List<String> d_Continents = new ArrayList<>();
    private HashMap<String, String> d_Country = new HashMap<>();
    private ConsoleWriter d_consoleWriter;

    public OpenMap() {
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
    }

    public void openMap(MapModel p_MapModel, String p_FileName) throws Exception{

        d_logger.setLogMessage("->Calling business file for loadmap\n");
        try {
            p_MapModel.clearMap();
            File l_Map = new File(ProjectConfig.D_MAP_FILES_PATH + p_FileName);
            FileReader l_FileReader = new FileReader(l_Map);

            d_Buffer = new BufferedReader(l_FileReader);
            while ((d_line = d_Buffer.readLine()) != null) {
                if (d_line.contains("[continents]")) {
                    while ((d_line = d_Buffer.readLine()) != null  && !d_line.contains("[")) {
                        if (d_line.isEmpty()) {
                            continue;
                        }
                        String[] l_ContinentDetails = d_line.split(" ");
                        p_MapModel.p_addContinent(l_ContinentDetails[0], l_ContinentDetails[1]);
                        d_Continents.add(l_ContinentDetails[0]);
                    }
                }
                if (d_line.contains("[countries]")) {
                    while ((d_line = d_Buffer.readLine()) != null  && !d_line.contains("[")) {
                        if (d_line.isEmpty()) {
                            continue;
                        }
                        String[] l_CountryDetails = d_line.split(" ");
                        p_MapModel.p_addCountry(l_CountryDetails[1], d_Continents.get((Integer.parseInt(l_CountryDetails[2]) - 1)));
                        d_Country.put(l_CountryDetails[0], l_CountryDetails[1]);
                    }
                }
                if (d_line.contains("[borders]")) {
                    while ((d_line = d_Buffer.readLine()) != null  && !d_line.contains("[")) {
                        if (d_line.isEmpty()) {
                            continue;
                        }
                        String[] l_NeighbourDetails = d_line.split(" ");
                        for (int i = 1; i < l_NeighbourDetails.length; i++) {
                            p_MapModel.p_addNeighbor(d_Country.get(l_NeighbourDetails[0]), d_Country.get(l_NeighbourDetails[i]));
                        }
                    }
                }

            }
        } catch (IOException e) {
            throw new Exception(e.getMessage());
        }
    }
}
