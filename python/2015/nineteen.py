def get_input(file_name="nineteen.input") -> tuple[dict[str,list[str]],str]:
    replacements = {}
    with open(file_name, "r") as f:
        body = f.read()
        replacements_body,molecule = body.split("\n\n")

        for replacement in replacements_body.split("\n"):
            (origin,to) = replacement.split(" => ")
            replacements[origin] = replacements.get(origin,[]) + [to]

    return replacements, molecule.strip()

def part_one(replacements,molecule):
    created_molecules = set()

    def handle_substr(start_i,end_i):
        if molecule[start_i:end_i] in replacements:
            for replacement in replacements[molecule[start_i:end_i]]:
                created_molecules.add(molecule[:start_i]+replacement+molecule[end_i:])

    for i in range(len(molecule)):
        handle_substr(i,i+1)
        if i != (len(molecule)-1):
            handle_substr(i,i+2)

    print(f"Part One Solution {len(created_molecules)}")

def part_two(replacements,target):
    # Breadth first search
    created_molecules = [(0,"e")]

    def handle_substr(start_i,end_i,molecule_pair):
        depth,molecule = molecule_pair
        if molecule[start_i:end_i] in replacements:
            for replacement in sorted(replacements[molecule[start_i:end_i]],key=lambda x: len(x),reverse=True):
                new_molecule = molecule[:start_i]+replacement+molecule[end_i:]
                if new_molecule == target:
                    print(f"Found at depth {depth+1}")
                    return True
                else:
                    if len(new_molecule) < len(target):
                        created_molecules.append((depth+1,new_molecule))
    substr_lens = list(set([len(key) for key in replacements.keys()]))
    while True:
        molecule = created_molecules.pop(-1)
        print(molecule)
        for i in range(len(molecule[1])):
            for substr_len in substr_lens:
                if i+substr_len <= len(molecule[1]):
                    if handle_substr(i,i+substr_len,molecule):
                        return

    
    


if __name__ == "__main__":
    replacements, molecule = get_input()
    part_one(replacements, molecule)
    part_two(replacements, molecule)
