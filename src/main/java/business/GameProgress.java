package business;

import GamePhase.MapPhaseState;
import logger.ConsoleWriter;
import logger.LogEntryBuffer;
import logger.LogGenerator;
import model.GameModel;
import model.MapModel;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Scanner;

public class GameProgress implements Serializable{

    static final String PATH = "savedFiles/";

    private LogEntryBuffer d_logger;
    private ConsoleWriter d_consoleWriter;
    private LogGenerator d_logGenrator;
    static MapModel mapModel;
    private final static Scanner l_Scanner = new Scanner(System.in);


    public GameProgress() {
        d_logger = new LogEntryBuffer();
        d_consoleWriter = new ConsoleWriter();
        d_logGenrator = LogGenerator.getInstance();
        d_logger.addObserver(d_consoleWriter);
        d_logger.addObserver(d_logGenrator);
        mapModel = MapModel.getInstance();
    }

    public boolean SaveGame(GameModel p_GameModel, MapModel p_MapModel, String p_Name){
        try {
            ObjectOutputStream outputStream =
                    new ObjectOutputStream (new FileOutputStream(PATH + p_Name + ".game"));
            ObjectOutputStream outputStream1 =
                    new ObjectOutputStream (new FileOutputStream(PATH + p_Name + "1.game"));
            outputStream.writeObject(p_MapModel);
            outputStream1.writeObject(p_GameModel);
            System.out.println("Game Saved successfully.");
            outputStream.close();
            outputStream1.close();
            return true;

        } catch (IOException e) {
            e.fillInStackTrace();
            return false;
        }
    }

    public void LoadGame(String p_fileName) throws IOException {
        showLoadFiles();
        String l_Command = l_Scanner.nextLine();
        String l_FileName = parseCommand(l_Command);

        String[] parts = l_FileName.split("\\.", 2);

        String l_FileName1 = parts[0] + "1.game";

        FileInputStream l_Fs;
        FileInputStream l_Fs1;

        GameModel l_loadedGameModel;
        MapModel l_loadedMapModel;
        try {
            l_Fs = new FileInputStream(PATH + l_FileName);
            l_Fs1 = new FileInputStream(PATH + l_FileName1);

            ObjectInputStream l_Os = new ObjectInputStream(l_Fs);
            ObjectInputStream l_Os1 = new ObjectInputStream(l_Fs1);

            l_loadedMapModel = (MapModel) l_Os.readObject();
            l_loadedGameModel = (GameModel) l_Os1.readObject();

            MapPhaseState.D_CURRENT_MAP = l_loadedMapModel.getMapName();
            d_logger.setLogMessage("The game is loaded successfully will continue from where it last stopped.");

            l_Os.close();
            l_Os1.close();

            MapModel.getInstance().MapModelBuilder(l_loadedMapModel);
            GameModel.getInstance().GameModelBuilder(l_loadedGameModel);

        } catch (IOException | ClassNotFoundException p_Exception) {
            d_logger.setLogMessage("The file could not be loaded.");
        }
    }

    public void showLoadFiles() throws IOException {
        d_logger.setLogMessage("****************************************");
        d_logger.setLogMessage("************** LOAD GAME ***************");
        d_logger.setLogMessage("****************************************");
        if (new File(PATH).exists()) {
            Files.walk(Path.of(PATH))
                    .filter(path -> path.toFile().isFile())
                    .forEach(path -> {
                        d_logger.setLogMessage("\t\t " + path.getFileName());
                    });
        } else {
            d_logger.setLogMessage("\t\t " + "no load files found");
        }
        d_logger.setLogMessage("");
        d_logger.setLogMessage("****************************************");
        d_logger.setLogMessage("******* Use file name to proceed ******");
        d_logger.setLogMessage("****************************************");
    }

    private String parseCommand(String command) {
        String[] l_Commands = command.split(" ");
        if (l_Commands.length == 2 && l_Commands[0].equals("loadgame")) {
            return l_Commands[1];
        }
        return "";
    }

}
