# Updated Stick_Diagram.py

import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, FancyArrowPatch
import matplotlib.lines as mlines

# Function to create a legend
def create_legend(ax):
    legend_patches = [
        mlines.Line2D([], [], color='blue', linewidth=2, label='12F'),
        mlines.Line2D([], [], color='green', linewidth=4, label='24F'),
        mlines.Line2D([], [], color='orange', linewidth=6, label='48F'),
        mlines.Line2D([], [], color='red', linewidth=8, label='96F+')
    ]
    ax.legend(handles=legend_patches, loc='lower left', title='Fiber Counts')

# Function to update connector colors
def update_connector_colors():
    return {
        '12F': 'blue',
        '24F': 'green',
        '48F': 'orange',
        '96F': 'red'
    }

# Function to draw nodes with updated colors
def draw_nodes(ax):
    ax.add_patch(Rectangle((1, 1), 1, 1, color='orange'))  # Regular Node
    ax.add_patch(Rectangle((3, 3), 1, 1, color='green', linestyle='--'))  # MDU/FUTURE
    ax.add_patch(Rectangle((5, 5), 1, 1, color='red'))  # SPLC

# New Option 4: Auto-fit to 11x17 with cable callouts
def auto_fit_and_callouts(ax):
    # Fit the view to a specific size
    ax.set_xlim(0, 11)
    ax.set_ylim(0, 17)
    # Adding callouts with arrows
    ax.annotate('Callout', xy=(5, 5), xytext=(6, 6),
                arrowprops=dict(arrowstyle='->', color='black'))

# Main function
def main():
    fig, ax = plt.subplots(figsize=(11, 17))
    draw_nodes(ax)
    create_legend(ax)
    auto_fit_and_callouts(ax)
    plt.title('Updated Stick Diagram')
    plt.show()

if __name__ == '__main__':
    main()