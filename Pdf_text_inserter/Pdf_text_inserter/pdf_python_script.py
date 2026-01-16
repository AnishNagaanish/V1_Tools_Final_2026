import fitz  # PyMuPDF
import os
from tkinter import Tk, filedialog, simpledialog

def insert_text_in_all_pdfs(folder_path, output_folder, date_text, day_text, month_text):
    os.makedirs(output_folder, exist_ok=True)

    text_items = [
        # Date text - First rectangle (customize width and height)
        {"page": 0, "text": date_text, "x": 420, "y": 730, "font_size": 12, 
         "bold": False, "italic": False, 
         "rect_width": 100, "rect_height": 15},
        
        # Day text - Second rectangle (different size)
        {"page": 1, "text": day_text, "x": 330, "y": 122, "font_size": 12, 
         "bold": False, "italic": False, 
         "rect_width": 20, "rect_height": 18},
        
        # Month text - Third rectangle (different size)
        {"page": 1, "text": month_text, "x": 420, "y": 122, "font_size": 12, 
         "bold": False, "italic": False, 
         "rect_width": 65, "rect_height": 20}
    ]

    for filename in os.listdir(folder_path):
        if filename.lower().endswith(".pdf"):
            input_pdf_path = os.path.join(folder_path, filename)
            output_pdf_path = os.path.join(output_folder, filename)

            pdf_document = fitz.open(input_pdf_path)

            for item in text_items:
                if item["page"] < len(pdf_document):
                    page = pdf_document[item["page"]]

                    # Select font
                    if item["bold"] and item["italic"]:
                        font_name = "Helvetica-BoldOblique"
                    elif item["bold"]:
                        font_name = "Helvetica-Bold"
                    elif item["italic"]:
                        font_name = "Helvetica-Oblique"
                    else:
                        font_name = "Helvetica"

                    # Draw white filled rectangle (each text has its own size)
                    rect = fitz.Rect(item["x"] - 2, item["y"] - item["rect_height"], 
                                    item["x"] + item["rect_width"], item["y"] + 2)
                    page.draw_rect(rect, color=(1, 1, 1), fill=(1, 1, 1))  # White background

                    # Insert new text on top of white rectangle
                    page.insert_text((item["x"], item["y"]), item["text"],
                                     fontsize=item["font_size"], fontname=font_name, fill=(0, 0, 0))

            pdf_document.save(output_pdf_path)
            pdf_document.close()
            print(f"Processed: {filename} → {output_pdf_path}")

# Use tkinter to select folders and input date
root = Tk()
root.withdraw()

print("Select the folder containing PDFs:")
input_folder = filedialog.askdirectory(title="Select Input Folder")

print("Select the output folder:")
output_folder = filedialog.askdirectory(title="Select Output Folder")

# Ask for three text inputs
date_text = simpledialog.askstring("Input Date", "Enter the date to insert (e.g., 11/11/2025):")
day_text = simpledialog.askstring("Input Day", "Enter the day to insert (e.g., 11):")
month_text = simpledialog.askstring("Input Month", "Enter the month to insert (e.g., November):")

insert_text_in_all_pdfs(input_folder, output_folder, date_text, day_text, month_text)