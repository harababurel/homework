package view;
import ctrl.*;
import repo.*;
import models.*;
import java.util.*;

public class DeserializeCommand extends Command {
    public DeserializeCommand() {
        super("d", "deserialize a Repository from some file");
    }

    @Override
    public void execute() {
        Scanner scanner = new Scanner(System.in);

        System.out.printf("Serialized file name: ");
        String filename = scanner.nextLine();

        Repository deserializedRepo = Repository.deserialize(filename);
        System.out.println("Deserialized the following repository:");
        System.out.println(deserializedRepo.toString());
    }
}
